-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Names (
    desugarImports
  , Exports()
  , ImportEnvironment()
  ) where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Either
import Data.Tuple
import Data.Tuple3
import Data.Foldable (elem, notElem, lookup, foldl)
import Data.Traversable (traverse)

import Control.Bind
import Control.Apply
import Control.Monad

import Control.Monad.Error
import Control.Monad.Error.Class

import qualified Data.Map as M

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Traversals
import Language.PureScript.Pos

-- |
-- The global export environment - every declaration exported from every module.
--
type ExportEnvironment = M.Map ModuleName Exports

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  {
  -- |
  -- The types exported from each module
  --
    exportedTypes :: [Tuple ProperName [ProperName]]
  -- |
  -- The classes exported from each module
  --
  , exportedTypeClasses :: [ProperName]
  -- |
  -- The values exported from each module
  , exportedValues :: [Ident]
  --
  }

mkExports :: [Tuple ProperName [ProperName]] ->
             [ProperName] ->
             [Ident] ->
             Exports
mkExports exportedTypes exportedTypeClasses exportedValues =
  Exports { exportedTypes: exportedTypes
          , exportedTypeClasses: exportedTypeClasses
          , exportedValues: exportedValues
          }

instance showExports :: Show Exports where
  show (Exports o) = "Exports "
    ++ "{ exportedTypes: " ++ show o.exportedTypes
    ++ ", exportedTypeClasses: " ++ show o.exportedTypeClasses
    ++ ", exportedValues: " ++ show o.exportedValues
    ++ "}"

-- |
-- An imported environment for a particular module. This also contains the module's own members.
--
data ImportEnvironment = ImportEnvironment
  {
  -- |
  -- Local names for types within a module mapped to to their qualified names
  --
    importedTypes :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for data constructors within a module mapped to to their qualified names
  --
  , importedDataConstructors :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for classes within a module mapped to to their qualified names
  --
  , importedTypeClasses :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for values within a module mapped to to their qualified names
  --
  , importedValues :: M.Map (Qualified Ident) (Qualified Ident)
  }

mkImportEnvironment :: M.Map (Qualified ProperName) (Qualified ProperName) ->
                       M.Map (Qualified ProperName) (Qualified ProperName) ->
                       M.Map (Qualified ProperName) (Qualified ProperName) ->
                       M.Map (Qualified Ident) (Qualified Ident) ->
                       ImportEnvironment
mkImportEnvironment importedTypes importedDataConstructors importedTypeClasses importedValues =
  ImportEnvironment { importedTypes: importedTypes
                    , importedDataConstructors: importedDataConstructors
                    , importedTypeClasses: importedTypeClasses
                    , importedValues: importedValues
                    }

instance showImportEnvironment :: Show ImportEnvironment where
  show (ImportEnvironment o) = "ImportEnvironment "
    ++ "{ importedTypes: " ++ show o.importedTypes
    ++ ", importedDataConstructors: " ++ show o.importedDataConstructors
    ++ ", importedTypeClasses: " ++ show o.importedTypeClasses
    ++ ", importedValues: " ++ show o.importedValues
    ++ "}"

-- |
-- Updates the exports for a module from the global environment. If the module was not previously
-- present in the global environment, it is created.
--
updateExportedModule :: ExportEnvironment -> ModuleName -> (Exports -> Either ErrorStack Exports) -> Either ErrorStack ExportEnvironment
updateExportedModule env mn update = do
  let exports = fromJust $ mn `M.lookup` env
  exports' <- update exports
  return $ M.insert mn exports' env

-- |
-- Adds an empty module to an ExportEnvironment.
--
addEmptyModule :: ExportEnvironment -> ModuleName -> Either ErrorStack ExportEnvironment
addEmptyModule env name =
  if name `M.member` env
    then throwError $ mkErrorStack ("Module '" ++ show name ++ "' has been defined more than once") Nothing
    else return $ M.insert name (mkExports [] [] []) env

-- |
-- Adds a type belonging to a module to the export environment.
--
addType :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> Either ErrorStack ExportEnvironment
addType env mn name dctors = updateExportedModule env mn $ \(Exports o) -> do
  types' <- addExport o.exportedTypes (Tuple name dctors)
  return $ Exports (o { exportedTypes = types' })

-- |
-- Adds a class to the export environment.
--
addTypeClass :: ExportEnvironment -> ModuleName -> ProperName -> Either ErrorStack ExportEnvironment
addTypeClass env mn name = updateExportedModule env mn $ \(Exports o) -> do
  classes <- addExport o.exportedTypeClasses name
  return $ Exports (o { exportedTypeClasses = classes })

-- |
-- Adds a class to the export environment.
--
addValue :: ExportEnvironment -> ModuleName -> Ident -> Either ErrorStack ExportEnvironment
addValue env mn name = updateExportedModule env mn $ \(Exports o) -> do
  values <- addExport o.exportedValues name
  return $ Exports (o { exportedValues = values })

-- |
-- Adds an entry to a list of exports unless it is already present, in which case an error is
-- returned.
--
addExport :: forall a. (Eq a, Show a) => [a] -> a -> Either ErrorStack [a]
addExport exports name =
  if name `elem` exports
  then throwError $ mkErrorStack ("Multiple definitions for '" ++ show name ++ "'") Nothing
  else return $ name : exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
desugarImports :: [Module] -> Either ErrorStack [Module]
desugarImports modules = do
  unfilteredExports <- findExports modules
  exports <- foldM filterModuleExports unfilteredExports modules
  traverse (renameInModule' unfilteredExports exports) modules
  where

  -- Filters the exports for a module in the global exports environment so that only explicitly
  -- exported members remain. If the module does not explicitly export anything, everything is
  -- exported.
  filterModuleExports :: ExportEnvironment -> Module -> Either ErrorStack ExportEnvironment
  filterModuleExports env (Module mn _ (Just exps)) = filterExports mn exps env
  filterModuleExports env _ = return env

  -- Rename and check all the names within a module. We tweak the global exports environment so
  -- the module has access to an unfiltered list of its own members.
  renameInModule' :: ExportEnvironment -> ExportEnvironment -> Module -> Either ErrorStack Module
  renameInModule' unfilteredExports exports m@(Module mn _ _) =
    rethrow (\e -> strMsg ("Error in module " ++ show mn) <> (e :: ErrorStack)) $ do
      let env = M.update (\_ -> M.lookup mn unfilteredExports) mn exports
      let exps = fromJust $ M.lookup mn exports
      imports <- resolveImports env m
      renameInModule imports env (elaborateExports exps m)

-- |
-- Make all exports for a module explicit. This may still effect modules that have an exports list,
-- as it will also make all data constructor exports explicit.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps@(Exports o) (Module mn decls _) = Module mn decls (Just $
  map (\(Tuple ctor dctors) -> TypeRef ctor (Just dctors)) o.exportedTypes ++
  map TypeClassRef o.exportedTypeClasses ++
  map ValueRef o.exportedValues)

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: ImportEnvironment -> ExportEnvironment -> Module -> Either ErrorStack Module
renameInModule imports exports (Module mn decls exps) =
  Module mn <$> traverse go decls <*> pure exps
  where
  go = (everywhereWithContextOnValuesM (Tuple Nothing []) updateDecl updateValue updateBinder updateCase defS).decls
  updateDecl :: (Tuple (Maybe SourcePos) [Ident]) -> Declaration -> Either ErrorStack (Tuple (Tuple (Maybe SourcePos) [Ident]) Declaration)
  updateDecl (Tuple _ bound) d@(PositionedDeclaration pos _) =
    return (Tuple (Tuple (Just pos) bound) d)
  updateDecl (Tuple pos bound) (DataDeclaration name args dctors) =
    Tuple (Tuple pos bound) <$> (DataDeclaration name args <$> traverse (sndM (traverse (updateTypesEverywhere pos))) dctors)
  updateDecl (Tuple pos bound) (TypeSynonymDeclaration name ps ty) =
    Tuple (Tuple pos bound) <$> (TypeSynonymDeclaration name ps <$> updateTypesEverywhere pos ty)
  updateDecl (Tuple pos bound) (TypeClassDeclaration className args implies ds) =
    Tuple (Tuple pos bound) <$> (TypeClassDeclaration className args <$> updateConstraints pos implies <*> pure ds)
  updateDecl (Tuple pos bound) (TypeInstanceDeclaration name cs cn ts ds) =
    Tuple (Tuple pos bound) <$> (TypeInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn pos <*> traverse (updateTypesEverywhere pos) ts <*> pure ds)
  updateDecl (Tuple pos bound) (ExternInstanceDeclaration name cs cn ts) =
    Tuple (Tuple pos bound) <$> (ExternInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn Nothing <*> traverse (updateTypesEverywhere pos) ts)
  updateDecl (Tuple pos bound) (TypeDeclaration name ty) =
    Tuple (Tuple pos bound) <$> (TypeDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl (Tuple pos bound) (ExternDeclaration fit name js ty) =
    Tuple (Tuple pos (name : bound)) <$> (ExternDeclaration fit name js <$> updateTypesEverywhere pos ty)
  updateDecl s d = return (Tuple s d)

  updateValue :: (Tuple (Maybe SourcePos) [Ident]) -> Value -> Either ErrorStack (Tuple (Tuple (Maybe SourcePos) [Ident]) Value)
  updateValue (Tuple _ bound) v@(PositionedValue pos' _) =
    return (Tuple (Tuple (Just pos') bound) v)
  updateValue (Tuple pos bound) (Abs (Left arg) val') =
    return (Tuple (Tuple pos (arg : bound)) (Abs (Left arg) val'))
  updateValue (Tuple pos bound) (Let ds val') =
    let args = mapMaybe letBoundVariable ds
    in return (Tuple (Tuple pos (args ++ bound)) (Let ds val'))
  updateValue (Tuple pos bound) (Var name'@(Qualified Nothing ident)) | ident `notElem` bound =
    Tuple (Tuple pos bound) <$> (Var <$> updateValueName name' pos)
  updateValue (Tuple pos bound) (Var name'@(Qualified (Just _) _)) =
    Tuple (Tuple pos bound) <$> (Var <$> updateValueName name' pos)
  updateValue (Tuple pos bound) (BinaryNoParens name'@(Qualified Nothing ident) v1 v2) | ident `notElem` bound =
    Tuple (Tuple pos bound) <$> (BinaryNoParens <$> updateValueName name' pos <*> pure v1 <*> pure v2)
  updateValue (Tuple pos bound) (BinaryNoParens name'@(Qualified (Just _) _) v1 v2) =
    Tuple (Tuple pos bound) <$> (BinaryNoParens <$> updateValueName name' pos <*> pure v1 <*> pure v2)
  updateValue s@(Tuple pos _) (Constructor name) =
    Tuple s <$> (Constructor <$> updateDataConstructorName name pos)
  updateValue s@(Tuple pos _) (TypedValue check val ty) =
    Tuple s <$> (TypedValue check val <$> updateTypesEverywhere pos ty)
  updateValue s v = return (Tuple s v)

  updateBinder :: (Tuple (Maybe SourcePos) [Ident]) -> Binder -> Either ErrorStack (Tuple (Tuple (Maybe SourcePos) [Ident]) Binder)
  updateBinder (Tuple _ bound) v@(PositionedBinder pos _) =
    return (Tuple (Tuple (Just pos) bound) v)
  updateBinder s@(Tuple pos _) (ConstructorBinder name b) =
    Tuple s <$> (ConstructorBinder <$> updateDataConstructorName name pos <*> pure b)
  updateBinder s v = return (Tuple s v)

  updateCase :: (Tuple (Maybe SourcePos) [Ident]) -> CaseAlternative -> Either ErrorStack (Tuple (Tuple (Maybe SourcePos) [Ident]) CaseAlternative)
  updateCase (Tuple pos bound) c@(CaseAlternative o) = return (Tuple (Tuple pos (concatMap binderNames o.binders ++ bound)) c)

  letBoundVariable :: Declaration -> Maybe Ident
  letBoundVariable (ValueDeclaration ident _ _ _ _) = Just ident
  letBoundVariable (PositionedDeclaration _ d) = letBoundVariable d
  letBoundVariable _ = Nothing

  updateTypesEverywhere :: Maybe SourcePos -> Type -> Either ErrorStack Type
  updateTypesEverywhere pos0 = everywhereOnTypesM (updateType pos0)
    where
    updateType :: Maybe SourcePos -> Type -> Either ErrorStack Type
    updateType pos (TypeConstructor name) = TypeConstructor <$> updateTypeName name pos
    updateType pos (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym <$> updateTypeName name pos <*> pure tys
    updateType pos (ConstrainedType cs t) = ConstrainedType <$> updateConstraints pos cs <*> pure t
    updateType _ t = return t

  updateConstraints pos = traverse (\(Tuple name ts) -> Tuple <$> updateClassName name pos <*> traverse (updateTypesEverywhere pos) ts)

  updateTypeName = update "type" (\(ImportEnvironment o) -> o.importedTypes) (\(mes@(Exports o)) -> isJust <<< (\x -> x `lookup` o.exportedTypes))
  updateClassName = update "type class" (\(ImportEnvironment o) -> o.importedTypeClasses) (\(Exports o) x -> x `elem` o.exportedTypeClasses)
  updateValueName = update "value" (\(ImportEnvironment o) -> o.importedValues) (\(Exports o) x -> x `elem` o.exportedValues)
  updateDataConstructorName = update "data constructor" (\(ImportEnvironment o) -> o.importedDataConstructors) (\(mes@(Exports o)) -> flip elem (join $ snd `map` o.exportedTypes))

  -- Update names so unqualified references become qualified, and locally qualified references
  -- are replaced with their canoncial qualified names (e.g. M.Map -> Data.Map.Map)
  update :: forall a. (Ord a, Show a) =>
                            String
                            -> (ImportEnvironment -> M.Map (Qualified a) (Qualified a))
                            -> (Exports -> a -> Boolean)
                            -> Qualified a
                            -> Maybe SourcePos
                            -> Either ErrorStack (Qualified a)
  update t getI checkE qname@(Qualified mn' name) pos = case (Tuple (M.lookup qname (getI imports)) mn') of
    (Tuple (Just qname') _) -> return qname'
    (Tuple Nothing (Just mn'')) -> do
      modExports <- getExports mn''
      if checkE modExports name
        then return qname
        else positioned $ throwError $ mkErrorStack ("Unknown " ++ t ++ " '" ++ show qname ++ "'") Nothing
    _ -> positioned $ throwError $ mkErrorStack ("Unknown " ++ t ++ " '" ++ show name ++ "'") Nothing
    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

  -- Gets the exports for a module, or an error message if the module doesn't exist
  getExports :: ModuleName -> Either ErrorStack Exports
  getExports mn' = maybe (throwError $ mkErrorStack ("Unknown module '" ++ show mn' ++ "'") Nothing) return $ M.lookup mn' exports

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: [Module] -> Either ErrorStack ExportEnvironment
findExports = foldM addModule $ M.singleton (ModuleName [ProperName "Prim"]) primExports
  where

  -- The exported types from the Prim module
  primExports = mkExports (mkTypeEntry `map` M.keys primTypes) [] []
    where
    mkTypeEntry (Qualified _ name) = (Tuple name [])

  -- Add all of the exported declarations from a module to the global export environment
  addModule :: ExportEnvironment -> Module -> Either ErrorStack ExportEnvironment
  addModule env (Module mn ds _) = do
    env' <- addEmptyModule env mn
    rethrow (\e -> strMsg ("Error in module " ++ show mn) <> (e :: ErrorStack)) $ foldM (addDecl mn) env' ds

  -- Add a declaration from a module to the global export environment
  addDecl :: ModuleName -> ExportEnvironment -> Declaration -> Either ErrorStack ExportEnvironment
  addDecl mn env (TypeClassDeclaration tcn _ _ ds) = do
    env' <- addTypeClass env mn tcn
    foldM go env' ds
    where
    go env'' (TypeDeclaration name _) = addValue env'' mn name
    go env'' (PositionedDeclaration pos d) = rethrowWithPosition pos $ go env'' d
    go _ _ = theImpossibleHappened "Invalid declaration in TypeClassDeclaration"
  addDecl mn env (DataDeclaration tn _ dcs) = addType env mn tn (map fst dcs)
  addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn []
  addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn []
  addDecl mn env (ValueDeclaration name _ _ _ _) = addValue env mn name
  addDecl mn env (ExternDeclaration _ name _ _) = addValue env mn name
  addDecl mn env (PositionedDeclaration _ d) = addDecl mn env d
  addDecl _  env _ = return env

-- |
-- Filters the exports for a module to ensure only explicit exports are kept in the global exports
-- environment.
--
filterExports :: ModuleName -> [DeclarationRef] -> ExportEnvironment -> Either ErrorStack ExportEnvironment
filterExports mn exps env = do
  let moduleExports = fromJust (mn `M.lookup` env)
  moduleExports' <- rethrow (\e -> strMsg ("Error in module " ++ show mn) <> (e :: ErrorStack)) $ filterModule moduleExports
  return $ M.insert mn moduleExports' env
  where

  -- Filter the exports for the specific module
  filterModule :: Exports -> Either ErrorStack Exports
  filterModule exported@(Exports o) = do
    types' <- foldM (filterTypes o.exportedTypes) [] exps
    values <- foldM (filterValues o.exportedValues) [] exps
    classes <- foldM (filterClasses o.exportedTypeClasses) [] exps
    return $ Exports (o { exportedTypes = types', exportedTypeClasses = classes, exportedValues = values })

  -- Ensure the exported types and data constructors exist in the module and add them to the set of
  -- exports
  filterTypes :: [Tuple ProperName [ProperName]] -> [Tuple ProperName [ProperName]] -> DeclarationRef -> Either ErrorStack [Tuple ProperName [ProperName]]
  filterTypes expTys result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterTypes expTys result r
  filterTypes expTys result (TypeRef name expDcons) = do
    dcons <- maybe (throwError $ mkErrorStack ("Cannot export undefined type '" ++ show name ++ "'") Nothing) return $ name `lookup` expTys
    dcons' <- maybe (return dcons) (foldM (filterDcons name dcons) []) expDcons
    return $ (Tuple name dcons') : result
  filterTypes _ result _ = return result

  -- Ensure the exported data constructors exists for a type and add them to the list of exports
  filterDcons :: ProperName -> [ProperName] -> [ProperName] -> ProperName -> Either ErrorStack [ProperName]
  filterDcons tcon exps' result name =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined data constructor '" ++ show name ++ "' for type '" ++ show tcon ++ "'") Nothing

  -- Ensure the exported classes exist in the module and add them to the set of exports
  filterClasses :: [ProperName] -> [ProperName] -> DeclarationRef -> Either ErrorStack [ProperName]
  filterClasses exps' result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterClasses exps' result r
  filterClasses exps' result (TypeClassRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined type class '" ++ show name ++ "'") Nothing
  filterClasses _ result _ = return result

  -- Ensure the exported values exist in the module and add them to the set of exports
  filterValues :: [Ident] -> [Ident] -> DeclarationRef -> Either ErrorStack [Ident]
  filterValues exps' result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterValues exps' result r
  filterValues exps' result (ValueRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined value '" ++ show name ++ "'") Nothing
  filterValues _ result _ = return result

-- |
-- Type representing a set of declarations being explicitly imported from a module
--
type ExplicitImports = [DeclarationRef]

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports :: [Declaration] -> M.Map ModuleName (Tuple3 (Maybe SourcePos) (Maybe ExplicitImports) (Maybe ModuleName))
findImports = foldl (findImports' Nothing) M.empty
  where
  findImports' pos result (ImportDeclaration mn expl qual) = M.insert mn (Tuple3 pos expl qual) result
  findImports' _   result (PositionedDeclaration pos d)    = findImports' (Just pos) result d
  findImports' _   result _                                = result

-- |
-- Constructs a local environment for a module.
--
resolveImports :: ExportEnvironment -> Module -> Either ErrorStack ImportEnvironment
resolveImports env (Module currentModule decls _) =
  foldM resolveImport' (mkImportEnvironment M.empty M.empty M.empty M.empty) (M.toList scope)
  where

  -- A Map from module name to the source position for the import, the list of imports from that
  -- module (where Nothing indicates everything is to be imported), and optionally a qualified name
  -- for the module
  scope :: M.Map ModuleName (Tuple3 (Maybe SourcePos) (Maybe ExplicitImports) (Maybe ModuleName))
  scope = M.insert currentModule (Tuple3 Nothing Nothing Nothing) (findImports decls)

  resolveImport' :: ImportEnvironment -> (Tuple ModuleName (Tuple3 (Maybe SourcePos) (Maybe ExplicitImports) (Maybe ModuleName))) -> Either ErrorStack ImportEnvironment
  resolveImport' imp (Tuple mn (Tuple3 pos explImports impQual)) = do
    modExports <- positioned $ maybe (throwError $ mkErrorStack ("Cannot import unknown module '" ++ show mn ++ "'") Nothing) return $ M.lookup mn env
    positioned $ resolveImport currentModule mn modExports imp impQual explImports
    where
    positioned :: forall a. Either ErrorStack a -> Either ErrorStack a
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport :: ModuleName -> ModuleName -> Exports -> ImportEnvironment -> Maybe ModuleName -> Maybe ExplicitImports -> Either ErrorStack ImportEnvironment
resolveImport currentModule importModule exps@(Exports expso) imps impQual = maybe importAll (foldM importExplicit imps)
  where

  -- Import everything from a module
  importAll :: Either ErrorStack ImportEnvironment
  importAll = do
    imp' <- foldM (\m (Tuple name dctors) -> importExplicit m (TypeRef name (Just dctors))) imps expso.exportedTypes
    imp'' <- foldM (\m name -> importExplicit m (ValueRef name)) imp' expso.exportedValues
    foldM (\m name -> importExplicit m (TypeClassRef name)) imp'' expso.exportedTypeClasses

  -- Import something explicitly
  importExplicit :: ImportEnvironment -> DeclarationRef -> Either ErrorStack ImportEnvironment
  importExplicit imp (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ importExplicit imp r
  importExplicit imp@(ImportEnvironment impo) (ValueRef name) = do
    _ <- checkImportExists "value" values name
    values' <- updateImports impo.importedValues name
    return $ ImportEnvironment (impo { importedValues = values' })
  importExplicit imp@(ImportEnvironment impo) (TypeRef name dctors) = do
    _ <- checkImportExists "type" availableTypes name
    types' <- updateImports impo.importedTypes name
    let allDctors = allExportedDataConstructors name
    dctors' <- maybe (return allDctors) (traverse $ checkDctorExists allDctors) dctors
    dctors'' <- foldM updateImports impo.importedDataConstructors dctors'
    return $ ImportEnvironment (impo { importedTypes = types', importedDataConstructors = dctors'' })
  importExplicit imp@(ImportEnvironment impo) (TypeClassRef name) = do
    _ <- checkImportExists "type class" classes name
    typeClasses' <- updateImports impo.importedTypeClasses name
    return $ ImportEnvironment (impo { importedTypeClasses = typeClasses' })
  importExplicit _ _ = theImpossibleHappened "Invalid argument to importExplicit"

  -- Find all exported data constructors for a given type
  allExportedDataConstructors :: ProperName -> [ProperName]
  allExportedDataConstructors name = fromMaybe [] $ name `lookup` expso.exportedTypes

  -- Add something to the ImportEnvironment if it does not already exist there
  updateImports :: forall a. (Ord a, Show a) => M.Map (Qualified a) (Qualified a) -> a -> Either ErrorStack (M.Map (Qualified a) (Qualified a))
  updateImports m name = case M.lookup (Qualified impQual name) m of
    Nothing -> return $ M.insert (Qualified impQual name) (Qualified (Just importModule) name) m
    Just (Qualified Nothing _) -> theImpossibleHappened "Invalid state in updateImports"
    Just x@(Qualified (Just mn) _) ->
      let
        err = if mn == currentModule || importModule == currentModule
              then "Definition '" ++ show name ++ "' conflicts with import '" ++ show (Qualified (Just importModule) name) ++ "'"
              else "Conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just importModule) name) ++ "'"
      in throwError $ mkErrorStack err Nothing

  -- The available values, types, and classes in the module being imported
  values = expso.exportedValues
  availableTypes = fst `map` expso.exportedTypes
  classes = expso.exportedTypeClasses

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists :: [ProperName] -> ProperName -> Either ErrorStack ProperName
  checkDctorExists = checkImportExists "data constructor"

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists :: forall a. (Eq a, Show a) => String -> [a] -> a -> Either ErrorStack a
  checkImportExists t exports item =
      if item `elem` exports
      then return item
      else throwError $ mkErrorStack ("Cannot import unknown " ++ t ++  " '" ++ show item ++ "' from '" ++ show importModule ++ "'") Nothing
