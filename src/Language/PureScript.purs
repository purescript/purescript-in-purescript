-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- The main compiler module
--
-----------------------------------------------------------------------------

module Language.PureScript (
    compile, 
    compile', 
    FilePath(..),
    MonadMake, 
    make
  ) where

import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Tuple3
import Data.Either
import Data.Function (on)
import Data.String (joinWith)
import Data.Foldable (all, any, elem, find, traverse_)
import Data.Traversable (for, traverse)

import qualified Data.Array.Unsafe as Unsafe
import qualified Data.Maybe.Unsafe as Unsafe

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.State.Class

import Control.Apply

import Math (min)

import Language.PureScript.Types 
import Language.PureScript.Kinds 
import Language.PureScript.Declarations 
import Language.PureScript.Names 
import Language.PureScript.Options 
import Language.PureScript.ModuleDependencies 
import Language.PureScript.Environment 
import Language.PureScript.Errors 
{- import Language.PureScript.DeadCodeElimination -}
import Language.PureScript.Supply 

import Language.PureScript.CodeGen.Common 
import Language.PureScript.CodeGen.JS
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.Externs 

import Language.PureScript.TypeChecker 
import Language.PureScript.TypeChecker.Monad

import Language.PureScript.Sugar 
import Language.PureScript.Sugar.BindingGroups

import Language.PureScript.Parser.Lexer (lex)
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Declarations (parseModules)

import Language.PureScript.Pretty.JS

import qualified Language.PureScript.Constants as C

foreign import pathSeparator "var pathSeparator = require('path').sep" :: String 

-- |
-- Compile a collection of modules
--
-- The compilation pipeline proceeds as follows:
--
--  * Sort the modules based on module dependencies, checking for cyclic dependencies.
--
--  * Perform a set of desugaring passes.
--
--  * Type check, and elaborate values to include type annotations and type class dictionaries.
--
--  * Regroup values to take into account new value dependencies introduced by elaboration.
--
--  * Eliminate dead code.
--
--  * Generate Javascript, and perform optimization passes.
--
--  * Pretty-print the generated Javascript
--
compile :: Options -> [Module] -> Either String (Tuple3 String String Environment)
compile = compile' initEnvironment

compile' :: Environment -> Options -> [Module] -> Either String (Tuple3 String String Environment)
compile' env opts@(Options optso) ms = do
  Tuple sorted _ <- sortModules $ if optso.noPrelude then ms else (map importPrelude ms)
  Tuple desugared nextVar <- stringifyErrorStack true $ runSupplyT 0 $ desugar sorted
  Tuple elaborated env' <- runCheck' opts env $ for desugared $ typeCheckModule mainModuleIdent
  regrouped <- stringifyErrorStack true $ createBindingGroupsModule <<< collapseBindingGroupsModule $ elaborated
  let 
    entryPoints = moduleNameFromString `map` optso.modules
    elim = regrouped -- if null entryPoints then regrouped else eliminateDeadCode entryPoints regrouped
    codeGenModules = moduleNameFromString `map` optso.codeGenModules
    modulesToCodeGen = if null codeGenModules then elim else filter (\(Module mn _ _) -> mn `elem` codeGenModules) elim
    js = evalSupply nextVar $ concat <$> traverse (\m -> moduleToJs Globals opts m env') modulesToCodeGen
    exts = joinWith "\n" <<< map (\m -> moduleToPs m env') $ modulesToCodeGen
  js' <- generateMain env' opts js
  return (Tuple3 (prettyPrintJS js') exts env')
  where
  mainModuleIdent = moduleNameFromString <$> optso.main

typeCheckModule :: Maybe ModuleName -> Module -> Check Module
typeCheckModule mainModuleName (Module mn decls exps) = do
  modify (\(CheckState st) -> CheckState (st { currentModule = Just mn }))
  decls' <- typeCheckAll mainModuleName mn decls
  traverse_ checkTypesAreExported exps'
  return $ Module mn decls' exps
  where

  exps' = Unsafe.fromJust exps

  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> Check {}
  checkTypesAreExported (ValueRef name) = do
    ty <- lookupVariable unifyError mn (Qualified (Just mn) name)
    case find isTconHidden (findTcons ty) of
      Just hiddenType -> throwError (strMsg ("Error in module '" ++ show mn ++ 
                                             "':\nExporting declaration '" ++ show name ++ 
                                             "' requires type '" ++ show hiddenType ++ 
                                             "' to be exported as well") :: ErrorStack)
      Nothing -> return {}
  checkTypesAreExported _ = return {}

  -- Find the type constructors exported from the current module used in a type
  findTcons :: Type -> [ProperName]
  findTcons = everythingOnTypes (++) go
    where
    go (TypeConstructor (Qualified (Just mn') name)) | mn' == mn = [name]
    go _ = []

  -- Checks whether a type constructor is not being exported from the current module
  isTconHidden :: ProperName -> Boolean
  isTconHidden tyName = all go exps'
    where
    go (TypeRef tyName' _) = tyName' /= tyName
    go _ = true

generateMain :: Environment -> Options -> [JS] -> Either String [JS]
generateMain env@(Environment envo) opts@(Options optso) js =
  case moduleNameFromString <$> optso.main of
    Just mmi -> do
      unless (Tuple mmi (Ident C.main) `M.member` envo.names) $
        Left $ show mmi ++ "." ++ C.main ++ " is undefined"
      return $ js ++ [JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar (Unsafe.fromJust optso.browserNamespace)))) []]
    _ -> return js

type FilePath = String

-- |
-- A type class which collects the IO actions we need to be able to run in "make" mode
--
class (Monad m) <= MonadMake m where
  -- |
  -- Get a file timestamp
  --
  getTimestamp :: FilePath -> m (Maybe Number)

  -- |
  -- Read a file as a string
  --
  readTextFile :: FilePath -> m String

  -- |
  -- Write a text file
  --
  writeTextFile :: FilePath -> String -> m {}

  -- |
  -- Report an error
  --
  liftError :: forall a. Either String a -> m a

  -- |
  -- Respond to a progress update
  --
  progress :: String -> m {}

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: forall m. (Functor m, Apply m, Applicative m, Bind m, Monad m, MonadMake m) => FilePath -> Options -> [Tuple FilePath Module] -> m Environment
make outputDir opts@(Options optso) ms = do
  let filePathMap = M.fromList (map (\(Tuple fp (Module mn _ _)) -> Tuple mn fp) ms)

  Tuple sorted graph <- liftError $ sortModules $ if optso.noPrelude then map snd ms else (map (importPrelude <<< snd) ms)

  toRebuild <- foldM (\s (Module moduleName' _ _) -> 
    do let filePath = runModuleName moduleName'
       
           jsFile      = outputDir ++ pathSeparator ++ filePath ++ pathSeparator ++ "index.js"
           externsFile = outputDir ++ pathSeparator ++ filePath ++ pathSeparator ++ "externs.purs"
           inputFile   = fromMaybe (error "Input file is undefined in make") $ M.lookup moduleName' filePathMap
       
       jsTimestamp      <- getTimestamp jsFile
       externsTimestamp <- getTimestamp externsFile
       inputTimestamp   <- getTimestamp inputFile
    
       return $ case (Tuple3 inputTimestamp jsTimestamp externsTimestamp) of
         Tuple3 (Just t1) (Just t2) (Just t3) | t1 < min t2 t3 -> s
         _ -> S.insert moduleName' s
    ) S.empty sorted
  
  marked <- rebuildIfNecessary (reverseDependencies graph) toRebuild sorted
  
  Tuple desugared nextVar <- liftError $ stringifyErrorStack true $ runSupplyT 0 $ zip (map fst marked) <$> desugar (map snd marked)
  
  evalSupplyT nextVar (go initEnvironment desugared)

  where
  go :: forall m. (Functor m, Apply m, Applicative m, Bind m, Monad m, MonadMake m) => Environment -> [Tuple Boolean Module] -> SupplyT m Environment
  go env [] = return env
  go env (Tuple false m : ms') = do
    Tuple _ env' <- lift (liftError (runCheck' opts env (typeCheckModule Nothing m)))
    go env' ms'
  go env (Tuple true (m@(Module moduleName' _ exps)) : ms') = do
    let filePath = runModuleName moduleName'
        jsFile = outputDir ++ pathSeparator ++ filePath ++ pathSeparator ++ "index.js"
        externsFile = outputDir ++ pathSeparator ++ filePath ++ pathSeparator ++ "externs.purs"
    
    lift (progress ("Compiling " ++ runModuleName moduleName'))
    
    Tuple (Module _ elaborated _) env' <- lift (liftError (runCheck' opts env (typeCheckModule Nothing m)))
    
    regrouped <- lift (liftError (stringifyErrorStack true (createBindingGroups moduleName' (collapseBindingGroups elaborated))))
    
    let mod' = Module moduleName' regrouped exps
    js <- prettyPrintJS <$> moduleToJs CommonJS opts mod' env'
    let exts = moduleToPs mod' env'
    
    lift $ writeTextFile jsFile js
    lift $ writeTextFile externsFile exts
    
    go env' ms'

  rebuildIfNecessary :: forall m. (Functor m, Apply m, Applicative m, Bind m, Monad m, MonadMake m) => M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [Tuple Boolean Module]
  rebuildIfNecessary _ _ [] = return []
  rebuildIfNecessary graph toRebuild ((m@(Module moduleName' _ _)) : ms') | moduleName' `S.member` toRebuild = do
    let deps = fromMaybe [] $ moduleName' `M.lookup` graph
        toRebuild' = toRebuild `S.union` S.fromList deps
    (:) (Tuple true m) <$> rebuildIfNecessary graph toRebuild' ms'
  rebuildIfNecessary graph toRebuild (Module moduleName' _ _ : ms') = do
    let externsFile = outputDir ++ pathSeparator ++ runModuleName moduleName' ++ pathSeparator ++ "externs.purs"
    externs <- readTextFile externsFile
    externsModules <- liftError (either (Left <<< show) Right (lex externs >>= runTokenParser parseModules))
    case externsModules of
      [m'@(Module moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (Tuple false m') <$> rebuildIfNecessary graph toRebuild ms'
      _ -> liftError (Left ("Externs file " ++ externsFile ++ " was invalid"))

reverseDependencies :: ModuleGraph -> M.Map ModuleName [ModuleName]
reverseDependencies g = combine $ do Tuple mn deps <- g
                                     dep <- deps
                                     return (Tuple dep mn)
  where
  combine :: forall a b. (Ord a) => [Tuple a b] -> M.Map a [b]
  combine = M.fromList <<< map (\xs -> Tuple (fst (Unsafe.head xs)) (map snd xs)) <<< groupBy ((==) `on` fst) <<< sortBy (compare `on` fst)

-- |
-- Add an import declaration for the Prelude to a module if it does not already explicitly import
-- it.
--
importPrelude :: Module -> Module
importPrelude m@(Module mn decls exps) =
  if any isPreludeImport decls || mn == prelude then m else Module mn (preludeImport : decls) exps
  where
  prelude :: ModuleName
  prelude = ModuleName [ProperName C.prelude]
  
  isPreludeImport :: Declaration -> Boolean
  isPreludeImport (ImportDeclaration (ModuleName [ProperName mn']) _ _) | mn' == C.prelude = true
  isPreludeImport (PositionedDeclaration _ d) = isPreludeImport d
  isPreludeImport _ = false
  
  preludeImport :: Declaration
  preludeImport = ImportDeclaration prelude Nothing Nothing