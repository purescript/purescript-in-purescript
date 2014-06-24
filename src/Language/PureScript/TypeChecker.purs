-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- The top-level type checker, which checks all declarations in a module.
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeChecker (typeCheckAll) where

import Data.Maybe
import Data.Array
import Data.Foldable (for_, foldl, foldr, traverse_)
import Data.Traversable (for, traverse)

import Data.Tuple
import Data.Tuple3

import qualified Data.Map as M

import Control.Monad
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Error
import Control.Monad.Error.Class

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Declarations
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Environment
import Language.PureScript.Errors

import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Types
import Language.PureScript.TypeChecker.Kinds

addDataType :: ModuleName -> ProperName -> [String] -> [Tuple ProperName [Type]] -> Kind -> Check Unit
addDataType moduleName name args dctors ctorKind = do
  modifyEnv $ \(Environment env) -> Environment (env { types = M.insert (Qualified (Just moduleName) name) (Tuple ctorKind (DataType args dctors)) env.types })
  for_ dctors $ \(Tuple dctor tys) ->
    rethrow (\e -> strMsg ("Error in data constructor " ++ show dctor) <> (e :: ErrorStack)) $
      addDataConstructor moduleName name args dctor tys

addDataConstructor :: ModuleName -> ProperName -> [String] -> ProperName -> [Type] -> Check Unit
addDataConstructor moduleName name args dctor tys = do
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = foldr function retTy tys
  let polyType = mkForAll args dctorTy
  modifyEnv $ \(Environment env) -> Environment (env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) (Tuple name polyType) env.dataConstructors })

addTypeSynonym :: ModuleName -> ProperName -> [String] -> Type -> Kind -> Check Unit
addTypeSynonym moduleName name args ty kind = do
  modifyEnv $ \(Environment env) -> Environment (env { types = M.insert (Qualified (Just moduleName) name) (Tuple kind TypeSynonym) env.types
                                                     , typeSynonyms = M.insert (Qualified (Just moduleName) name) (Tuple args ty) env.typeSynonyms })

valueIsNotDefined :: ModuleName -> Ident -> Check Unit
valueIsNotDefined moduleName name = do
  Environment env <- getEnv
  case M.lookup (Tuple moduleName name) env.names of
    Just _ -> throwError (strMsg (show name ++ " is already defined") :: ErrorStack)
    Nothing -> return unit

addValue :: ModuleName -> Ident -> Type -> NameKind -> Check Unit
addValue moduleName name ty nameKind =
  modifyEnv $ \(Environment env) -> Environment (env { names = M.insert (Tuple moduleName name) (Tuple ty nameKind) env.names })
  
addTypeClass :: ModuleName -> ProperName -> [String] -> [Tuple (Qualified ProperName) [Type]] -> [Declaration] -> Check Unit
addTypeClass moduleName pn args implies ds =
  let members = map toPair ds in
  modifyEnv $ \(Environment env) -> Environment (env { typeClasses = M.insert (Qualified (Just moduleName) pn) (Tuple3 args members implies) env.typeClasses })
  where
  toPair (TypeDeclaration ident ty) = Tuple ident ty
  toPair (PositionedDeclaration _ d) = toPair d
  toPair _ = theImpossibleHappened "Invalid declaration in TypeClassDeclaration"

addTypeClassDictionaries :: [TypeClassDictionaryInScope] -> Check Unit
addTypeClassDictionaries entries =
  let mentries = M.fromList $ 
    do (entry@(TypeClassDictionaryInScope { name = Qualified mn _ })) <- entries
       return (Tuple (Tuple (canonicalizeDictionary entry) mn) entry)
  in modifyEnv $ \(Environment env) -> Environment (env { typeClassDictionaries = env.typeClassDictionaries `M.union` mentries })

checkTypeClassInstance :: ModuleName -> Type -> Check Unit
checkTypeClassInstance _ (TypeVar _) = return unit
checkTypeClassInstance _ (TypeConstructor ctor) = do
  Environment env <- getEnv
  when (ctor `M.member` env.typeSynonyms) $ throwError (strMsg "Type synonym instances are disallowed" :: ErrorStack)
checkTypeClassInstance m (TypeApp t1 t2) = do
  checkTypeClassInstance m t1
  checkTypeClassInstance m t2
checkTypeClassInstance _ ty = throwError $ mkErrorStack "Type class instance head is invalid." (Just (TypeError ty))

-- |
-- Type check all declarations in a module
--
-- At this point, many declarations will have been desugared, but it is still necessary to
--
--  * Kind-check all types and add them to the @Environment@
--
--  * Type-check all values and add them to the @Environment@
--
--  * Bring type class instances into scope
--
--  * Process module imports
--
typeCheckAll :: Maybe ModuleName -> ModuleName -> [Declaration] -> Check [Declaration]
typeCheckAll _ _ [] = return []
typeCheckAll mainModuleName moduleName ((d@(DataDeclaration name args dctors)) : rest) = do
  rethrow (\e -> strMsg ("Error in type constructor " ++ show name) <> (e :: ErrorStack)) $ do
    ctorKind <- kindsOf true moduleName name args (concatMap snd dctors)
    addDataType moduleName name args dctors ctorKind
  ds <- typeCheckAll mainModuleName moduleName rest
  return (d : ds)
typeCheckAll mainModuleName moduleName ((d@(DataBindingGroupDeclaration tys)) : rest) = do
  rethrow (\e -> strMsg "Error in data binding group" <> (e :: ErrorStack)) $ do
    let syns = mapMaybe toTypeSynonym tys
    let dataDecls = mapMaybe toDataDecl tys
    (Tuple syn_ks data_ks) <- kindsOfAll moduleName syns (map (\(Tuple3 name args dctors) -> (Tuple3 name args (concatMap snd dctors))) dataDecls)
    for_ (zip dataDecls data_ks) $ \(Tuple (Tuple3 name args dctors) ctorKind) ->
      addDataType moduleName name args dctors ctorKind
    for_ (zip syns syn_ks) $ \(Tuple (Tuple3 name args ty) kind) ->
      addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
  where
  toTypeSynonym (TypeSynonymDeclaration nm args ty) = Just (Tuple3 nm args ty)
  toTypeSynonym (PositionedDeclaration _ d') = toTypeSynonym d'
  toTypeSynonym _ = Nothing
  toDataDecl (DataDeclaration nm args dctors) = Just (Tuple3 nm args dctors)
  toDataDecl (PositionedDeclaration _ d') = toDataDecl d'
  toDataDecl _ = Nothing
typeCheckAll mainModuleName moduleName ((d@(TypeSynonymDeclaration name args ty)) : rest) = do
  rethrow (\e -> strMsg ("Error in type synonym " ++ show name) <> (e :: ErrorStack)) $ do
    kind <- kindsOf false moduleName name args [ty]
    addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (TypeDeclaration _ _ : _) = theImpossibleHappened "Type declarations should have been removed"
typeCheckAll mainModuleName moduleName (ValueDeclaration name nameKind [] Nothing val : rest) = do
  d <- rethrow (\e -> strMsg ("Error in declaration " ++ show name) <> (e :: ErrorStack)) $ do
    valueIsNotDefined moduleName name
    [Tuple _ (Tuple val' ty)] <- typesOf mainModuleName moduleName [Tuple name val]
    addValue moduleName name ty nameKind
    return $ ValueDeclaration name nameKind [] Nothing val'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (ValueDeclaration _ _ _ _ _ : _) = theImpossibleHappened "Binders were not desugared"
typeCheckAll mainModuleName moduleName (BindingGroupDeclaration vals : rest) = do
  d <- rethrow (\e -> strMsg ("Error in binding group " ++ show (map (\(Tuple3 ident _ _) -> ident) vals)) <> (e :: ErrorStack)) $ do
    for_ (map (\(Tuple3 ident _ _) -> ident) vals) $ \name ->
      valueIsNotDefined moduleName name
    tys <- typesOf mainModuleName moduleName $ map (\(Tuple3 ident _ ty) -> (Tuple ident ty)) vals
    vals' <- for (zipWith (\(Tuple3 name nameKind _) (Tuple _ (Tuple val ty)) -> (Tuple name (Tuple3 val nameKind ty))) vals tys) $ \(Tuple name (Tuple3 val nameKind ty)) -> do
      addValue moduleName name ty nameKind
      return (Tuple3 name nameKind val)
    return $ BindingGroupDeclaration vals'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName ((d@(ExternDataDeclaration name kind)) : rest) = do
  modifyEnv $ \(Environment env) -> Environment (env { types = M.insert (Qualified (Just moduleName) name) (Tuple kind ExternData) env.types })
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName ((d@(ExternDeclaration importTy name _ ty)) : rest) = do
  rethrow (\e -> strMsg ("Error in foreign import declaration " ++ show name) <> (e :: ErrorStack)) $ do
    Environment env <- getEnv
    kind <- kindOf moduleName ty
    guardWith (strMsg "Expected kind *" :: ErrorStack) $ kind == Star
    case M.lookup (Tuple moduleName name) env.names of
      Just _ -> throwError (strMsg (show name ++ " is already defined") :: ErrorStack)
      Nothing -> putEnv (Environment (env { names = M.insert (Tuple moduleName name) (Tuple ty (Extern importTy)) env.names }))
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName ((d@(FixityDeclaration _ name)) : rest) = do
  ds <- typeCheckAll mainModuleName moduleName rest
  Environment env <- getEnv
  guardWith (strMsg ("Fixity declaration with no binding: " ++ name) :: ErrorStack) $ M.member (Tuple moduleName (Op name)) env.names
  return $ d : ds
typeCheckAll mainModuleName currentModule ((d@(ImportDeclaration moduleName _ _)) : rest) = do
  tcds <- getTypeClassDictionaries
  let instances = filter (\(TypeClassDictionaryInScope tcd) -> case tcd.name of Qualified (Just mn) _ -> moduleName == mn) tcds
  addTypeClassDictionaries $ map (\(tcd@(TypeClassDictionaryInScope tcdo)) -> 
    case tcdo.name of
      Qualified _ ident -> TypeClassDictionaryInScope (tcdo { name = Qualified (Just currentModule) ident
                                                            , ty = TCDAlias (canonicalizeDictionary tcd) })) instances
  ds <- typeCheckAll mainModuleName currentModule rest
  return $ d : ds
typeCheckAll mainModuleName moduleName ((d@(TypeClassDeclaration pn args implies tys)) : rest) = do
  addTypeClass moduleName pn args implies tys
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (TypeInstanceDeclaration dictName deps className tys _ : rest) = do
  typeCheckAll mainModuleName moduleName (ExternInstanceDeclaration dictName deps className tys : rest)
typeCheckAll mainModuleName moduleName ((d@(ExternInstanceDeclaration dictName deps className tys)) : rest) = do
  traverse_ (checkTypeClassInstance moduleName) tys
  for_ deps $ traverse_ (checkTypeClassInstance moduleName) <<< snd
  addTypeClassDictionaries [TypeClassDictionaryInScope { name: Qualified (Just moduleName) dictName
                                                       , className: className
                                                       , instanceTypes: tys 
                                                       , dependencies: Just deps
                                                       , ty: TCDRegular
                                                       }]
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (PositionedDeclaration pos d : rest) =
  rethrowExceptionWithPosition pos $ do
    (d' : rest') <- typeCheckAll mainModuleName moduleName (d : rest)
    return (PositionedDeclaration pos d' : rest')
