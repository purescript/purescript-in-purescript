-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.ModuleDependencies
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Provides the ability to sort modules based on module dependencies
--
-----------------------------------------------------------------------------

module Language.PureScript.ModuleDependencies (
    sortModules,
    ModuleGraph(..)
  ) where

import Data.Graph
import Data.Tuple
import Data.Tuple3
import Data.Tuple5
import Data.Either
import Data.Array (concatMap, map, nub, mapMaybe)
import Data.Maybe
import Data.Foldable (elem)
import Data.Traversable (traverse)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- A list of modules with their dependencies
--
type ModuleGraph = [Tuple ModuleName [ModuleName]]

-- |
-- Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules :: [Module] -> Either String (Tuple [ModuleName] ModuleGraph)
sortModules ms = do
  let moduleGraph = map (\(m@(Module mn ds _)) -> Tuple mn (nub (concatMap usedModules ds))) ms
  let verts = map fst moduleGraph
  let edges = do Tuple mn mns <- moduleGraph
                 mn' <- mns
                 return $ Edge mn mn'
  ms' <- traverse toModule $ scc $ Graph verts edges
  return $ Tuple ms' moduleGraph

-- |
-- Calculate a list of used modules based on explicit imports and qualified names
--
usedModules :: Declaration -> [ModuleName]
usedModules = case everythingOnValues (++) forDecls forValues (const []) (const []) (const []) of
  Tuple5 f _ _ _ _ -> nub <<< f
  where
  forDecls :: Declaration -> [ModuleName]
  forDecls (ImportDeclaration mn _ _) = [mn]
  forDecls _ = []

  forValues :: Value -> [ModuleName]
  forValues (Var (Qualified (Just mn) _)) = [mn]
  forValues (BinaryNoParens (Qualified (Just mn) _) _ _) = [mn]
  forValues (Constructor (Qualified (Just mn) _)) = [mn]
  forValues (TypedValue _ _ ty) = forTypes ty
  forValues _ = []

  forTypes :: Type -> [ModuleName]
  forTypes (TypeConstructor (Qualified (Just mn) _)) = [mn]
  forTypes (ConstrainedType cs _) = mapMaybe (\(Tuple (Qualified mn _) _) -> mn) cs
  forTypes _ = []

getModuleName :: Module -> ModuleName
getModuleName (Module mn _ _) = mn

-- |
-- Convert a strongly connected component of the module graph to a module
--
toModule :: [ModuleName] -> Either String ModuleName
toModule [m] = return m
toModule ms = Left $ "Cycle in module dependencies: " ++ show ms
