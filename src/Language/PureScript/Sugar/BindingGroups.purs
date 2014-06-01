-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BindingGroups
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which creates binding groups from sets of
-- mutually-recursive value declarations and mutually-recursive type declarations.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.BindingGroups (
    createBindingGroups,
    createBindingGroupsModule,
    collapseBindingGroups,
    collapseBindingGroupsModule
  ) where

import Data.Array
import Data.Maybe
import Data.Either
import Data.Foldable (all, find)
import Data.Traversable

import Data.Graph

import Data.Tuple
import Data.Tuple3
import Data.Tuple5

import Control.Apply

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule :: [Module] -> Either ErrorStack [Module]
createBindingGroupsModule = traverse $ \(Module name ds exps) -> Module name <$> createBindingGroups name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule = map $ \(Module name ds exps) -> Module name (collapseBindingGroups ds) exps

-- |
-- Replace all sets of mutually-recursive declarations with binding groups
--
createBindingGroups :: ModuleName -> [Declaration] -> Either ErrorStack [Declaration]
createBindingGroups moduleName ds = do
  values <- traverse (createBindingGroupsForValue moduleName) $ filter isValueDecl ds
  let dataDecls = filter isDataDecl ds
      declNamed pn = 
        case find (\d -> getProperName d == pn) dataDecls of
          Just d -> d
      dataVerts = map getProperName dataDecls
      dataEdges = do d1 <- dataDecls
                     d2 <- usedProperNames moduleName d1 `intersect` dataVerts
                     return $ Edge (getProperName d1) d2
  dataBindingGroupDecls <- traverse toDataBindingGroup $ scc' getProperName declNamed $ Graph dataDecls dataEdges
  let valueVerts = map getIdent values
      valueNamed ident = 
        case find (\v -> getIdent v == ident) values of
          Just v -> v
      valueEdges = do val1 <- values
                      val2 <- usedIdents moduleName val1 `intersect` valueVerts
                      return $ Edge (getIdent val1) val2
      bindingGroupDecls = map toBindingGroup $ scc' getIdent valueNamed $ Graph values valueEdges
  return $ filter isImportDecl ds ++
           filter isExternDataDecl ds ++
           filter isExternInstanceDecl ds ++
           dataBindingGroupDecls ++
           filter isTypeClassDeclaration ds ++
           filter isFixityDecl ds ++
           filter isExternDecl ds ++
           bindingGroupDecls

createBindingGroupsForValue :: ModuleName -> Declaration -> Either ErrorStack Declaration
createBindingGroupsForValue moduleName =
  case everywhereOnValuesTopDownM return go return of
    Tuple3 f _ _ -> f
  where
  go (Let ds val) = Let <$> createBindingGroups moduleName ds <*> pure val
  go other = return other

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups ds = 
  case everywhereOnValues id collapseBindingGroupsForValue id of
    Tuple3 f _ _ -> map f (concatMap go ds)
  where
  go (DataBindingGroupDeclaration ds) = ds
  go (BindingGroupDeclaration ds) = map (\(Tuple3 ident nameKind val) -> ValueDeclaration ident nameKind [] Nothing val) ds
  go (PositionedDeclaration pos d) = map (PositionedDeclaration pos) $ go d
  go other = [other]

collapseBindingGroupsForValue :: Value -> Value
collapseBindingGroupsForValue (Let ds val) = Let (collapseBindingGroups ds) val
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> Declaration -> [Ident]
usedIdents moduleName =
  case everythingOnValues (++) (const []) usedNames (const []) (const []) (const []) of
    Tuple5 f _ _ _ _ -> nub <<< f
  where
  usedNames :: Value -> [Ident]
  usedNames (Var (Qualified Nothing name)) = [name]
  usedNames (Var (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  usedNames _ = []

usedProperNames :: ModuleName -> Declaration -> [ProperName]
usedProperNames moduleName =
  case accumTypes (everythingOnTypes (++) usedNames) of
    Tuple5 f _ _ _ _ -> nub <<< f
  where
  usedNames :: Type -> [ProperName]
  usedNames (ConstrainedType constraints _) = flip mapMaybe constraints $ \qual ->
    case qual of
      Tuple (Qualified (Just moduleName') name) _ | moduleName == moduleName' -> Just name
      _ -> Nothing
  usedNames (TypeConstructor (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  usedNames _ = []

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _ _) = ident
getIdent (PositionedDeclaration _ d) = getIdent d
getIdent _ = theImpossibleHappened "Expected ValueDeclaration"

getProperName :: Declaration -> ProperName
getProperName (DataDeclaration pn _ _) = pn
getProperName (TypeSynonymDeclaration pn _ _) = pn
getProperName (PositionedDeclaration _ d) = getProperName d
getProperName _ = theImpossibleHappened "Expected DataDeclaration"

toBindingGroup :: SCC Declaration -> Declaration
toBindingGroup (AcyclicSCC d) = d
toBindingGroup (CyclicSCC [d]) = d
toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration $ map fromValueDecl ds'

toDataBindingGroup :: SCC Declaration -> Either ErrorStack Declaration
toDataBindingGroup (AcyclicSCC d) = return d
toDataBindingGroup (CyclicSCC [d]) = 
  case isTypeSynonym d of
    Just pn -> Left $ mkErrorStack ("Cycle in type synonym " ++ show pn) Nothing
    _ -> return d
toDataBindingGroup (CyclicSCC ds') | all (isJust <<< isTypeSynonym) ds' = Left $ mkErrorStack "Cycle in type synonyms" Nothing
toDataBindingGroup (CyclicSCC ds') = return $ DataBindingGroupDeclaration ds'

isTypeSynonym :: Declaration -> Maybe ProperName
isTypeSynonym (TypeSynonymDeclaration pn _ _) = Just pn
isTypeSynonym (PositionedDeclaration _ d) = isTypeSynonym d
isTypeSynonym _ = Nothing

fromValueDecl :: Declaration -> Tuple3 Ident NameKind Value
fromValueDecl (ValueDeclaration ident nameKind [] Nothing val) = Tuple3 ident nameKind val
fromValueDecl (ValueDeclaration _ _ _ _ _) = theImpossibleHappened "Binders should have been desugared"
fromValueDecl (PositionedDeclaration _ d) = fromValueDecl d
fromValueDecl _ = theImpossibleHappened "Expected ValueDeclaration"