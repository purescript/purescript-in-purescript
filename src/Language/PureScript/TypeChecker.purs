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
import Data.Foldable (for_, foldl, foldr)

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

addDataType :: ModuleName -> ProperName -> [String] -> [Tuple ProperName [Type]] -> Kind -> Check {}
addDataType moduleName name args dctors ctorKind = do
  modifyEnv $ \(Environment env) -> Environment (env { types = M.insert (Qualified (Just moduleName) name) (Tuple ctorKind (DataType args dctors)) env.types })
  for_ dctors $ \(Tuple dctor tys) ->
    rethrow (\e -> strMsg ("Error in data constructor " ++ show dctor) <> (e :: ErrorStack)) $
      addDataConstructor moduleName name args dctor tys

addDataConstructor :: ModuleName -> ProperName -> [String] -> ProperName -> [Type] -> Check {}
addDataConstructor moduleName name args dctor tys = do
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = foldr function retTy tys
  let polyType = mkForAll args dctorTy
  modifyEnv $ \(Environment env) -> Environment (env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) (Tuple name polyType) env.dataConstructors })

addTypeSynonym :: ModuleName -> ProperName -> [String] -> Type -> Kind -> Check {}
addTypeSynonym moduleName name args ty kind = do
  modifyEnv $ \(Environment env) -> Environment (env { types = M.insert (Qualified (Just moduleName) name) (Tuple kind TypeSynonym) env.types
                                                     , typeSynonyms = M.insert (Qualified (Just moduleName) name) (Tuple args ty) env.typeSynonyms })

valueIsNotDefined :: ModuleName -> Ident -> Check {}
valueIsNotDefined moduleName name = do
  Environment env <- getEnv
  case M.lookup (Tuple moduleName name) env.names of
    Just _ -> throwError (strMsg (show name ++ " is already defined") :: ErrorStack)
    Nothing -> return {}

addValue :: ModuleName -> Ident -> Type -> NameKind -> Check {}
addValue moduleName name ty nameKind =
  modifyEnv $ \(Environment env) -> Environment (env { names = M.insert (Tuple moduleName name) (Tuple ty nameKind) env.names })
  
addTypeClass :: ModuleName -> ProperName -> [String] -> [Tuple (Qualified ProperName) [Type]] -> [Declaration] -> Check {}
addTypeClass moduleName pn args implies ds =
  let members = map toPair ds in
  modifyEnv $ \(Environment env) -> Environment (env { typeClasses = M.insert (Qualified (Just moduleName) pn) (Tuple3 args members implies) env.typeClasses })
  where
  toPair (TypeDeclaration ident ty) = Tuple ident ty
  toPair (PositionedDeclaration _ d) = toPair d
  toPair _ = theImpossibleHappened "Invalid declaration in TypeClassDeclaration"

addTypeClassDictionaries :: [TypeClassDictionaryInScope] -> Check {}
addTypeClassDictionaries entries =
  let mentries = M.fromList $ 
    do (entry@(TypeClassDictionaryInScope { name = Qualified mn _ })) <- entries
       return (Tuple (Tuple (canonicalizeDictionary entry) mn) entry)
  in modifyEnv $ \(Environment env) -> Environment (env { typeClassDictionaries = env.typeClassDictionaries `M.union` mentries })

checkTypeClassInstance :: ModuleName -> Type -> Check {}
checkTypeClassInstance _ (TypeVar _) = return {}
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
foreign import typeCheckAll :: Maybe ModuleName -> ModuleName -> [Declaration] -> Check [Declaration]
{-
typeCheckAll _ _ [] = return []
typeCheckAll mainModuleName moduleName (d@(DataDeclaration name args dctors) : rest) = do
  rethrow (strMsg ("Error in type constructor " ++ show name) <>) $ do
    ctorKind <- kindsOf True moduleName name args (concatMap snd dctors)
    addDataType moduleName name args dctors ctorKind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(DataBindingGroupDeclaration tys) : rest) = do
  rethrow (strMsg "Error in data binding group" <>) $ do
    let syns = mapMaybe toTypeSynonym tys
    let dataDecls = mapMaybe toDataDecl tys
    (syn_ks, data_ks) <- kindsOfAll moduleName syns (map (\(name, args, dctors) -> (name, args, concatMap snd dctors)) dataDecls)
    for_ (zip dataDecls data_ks) $ \((name, args, dctors), ctorKind) ->
      addDataType moduleName name args dctors ctorKind
    for_ (zip syns syn_ks) $ \((name, args, ty), kind) ->
      addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
  where
  toTypeSynonym (TypeSynonymDeclaration nm args ty) = Just (nm, args, ty)
  toTypeSynonym (PositionedDeclaration _ d') = toTypeSynonym d'
  toTypeSynonym _ = Nothing
  toDataDecl (DataDeclaration nm args dctors) = Just (nm, args, dctors)
  toDataDecl (PositionedDeclaration _ d') = toDataDecl d'
  toDataDecl _ = Nothing
typeCheckAll mainModuleName moduleName (d@(TypeSynonymDeclaration name args ty) : rest) = do
  rethrow (strMsg ("Error in type synonym " ++ show name) <>) $ do
    kind <- kindsOf False moduleName name args [ty]
    addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (TypeDeclaration _ _ : _) = error "Type declarations should have been removed"
typeCheckAll mainModuleName moduleName (ValueDeclaration name nameKind [] Nothing val : rest) = do
  d <- rethrow (strMsg ("Error in declaration " ++ show name) <>) $ do
    valueIsNotDefined moduleName name
    [(_, (val', ty))] <- typesOf mainModuleName moduleName [(name, val)]
    addValue moduleName name ty nameKind
    return $ ValueDeclaration name nameKind [] Nothing val'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (ValueDeclaration{} : _) = error "Binders were not desugared"
typeCheckAll mainModuleName moduleName (BindingGroupDeclaration vals : rest) = do
  d <- rethrow (strMsg ("Error in binding group " ++ show (map (\(ident, _, _) -> ident) vals)) <>) $ do
    for_ (map (\(ident, _, _) -> ident) vals) $ \name ->
      valueIsNotDefined moduleName name
    tys <- typesOf mainModuleName moduleName $ map (\(ident, _, ty) -> (ident, ty)) vals
    vals' <- for (zipWith (\(name, nameKind, _) (_, (val, ty)) -> (name, val, nameKind, ty)) vals tys) $ \(name, val, nameKind, ty) -> do
      addValue moduleName name ty nameKind
      return (name, nameKind, val)
    return $ BindingGroupDeclaration vals'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(ExternDataDeclaration name kind) : rest) = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (kind, ExternData) (types env) }
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(ExternDeclaration importTy name _ ty) : rest) = do
  rethrow (strMsg ("Error in foreign import declaration " ++ show name) <>) $ do
    env <- getEnv
    kind <- kindOf moduleName ty
    guardWith (strMsg "Expected kind *") $ kind == Star
    case M.lookup (moduleName, name) (names env) of
      Just _ -> throwError . strMsg $ show name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert (moduleName, name) (ty, Extern importTy) (names env) })
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(FixityDeclaration _ name) : rest) = do
  ds <- typeCheckAll mainModuleName moduleName rest
  env <- getEnv
  guardWith (strMsg ("Fixity declaration with no binding: " ++ name)) $ M.member (moduleName, Op name) $ names env
  return $ d : ds
typeCheckAll mainModuleName currentModule (d@(ImportDeclaration moduleName _ _) : rest) = do
  tcds <- getTypeClassDictionaries
  let instances = filter (\tcd -> let Qualified (Just mn) _ = tcdName tcd in moduleName == mn) tcds
  addTypeClassDictionaries [ tcd { tcdName = Qualified (Just currentModule) ident, tcdType = TCDAlias (canonicalizeDictionary tcd) }
                           | tcd <- instances
                           , let (Qualified _ ident) = tcdName tcd
                           ]
  ds <- typeCheckAll mainModuleName currentModule rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(TypeClassDeclaration pn args implies tys) : rest) = do
  addTypeClass moduleName pn args implies tys
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (TypeInstanceDeclaration dictName deps className tys _ : rest) = do
  typeCheckAll mainModuleName moduleName (ExternInstanceDeclaration dictName deps className tys : rest)
typeCheckAll mainModuleName moduleName (d@(ExternInstanceDeclaration dictName deps className tys) : rest) = do
  mapM_ (checkTypeClassInstance moduleName) tys
  for_ deps $ mapM_ (checkTypeClassInstance moduleName) . snd
  addTypeClassDictionaries [TypeClassDictionaryInScope (Qualified (Just moduleName) dictName) className tys (Just deps) TCDRegular]
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (PositionedDeclaration pos d : rest) =
  rethrowWithPosition pos $ do
    (d' : rest') <- typeCheckAll mainModuleName moduleName (d : rest)
    return (PositionedDeclaration pos d' : rest')
-}
