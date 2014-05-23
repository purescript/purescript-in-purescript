-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CaseDeclarations
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.CaseDeclarations (
    desugarCases,
    desugarCasesModule
  ) where

import Data.Array (map, groupBy, length)
import qualified Data.Array.Unsafe as Unsafe

import Data.Maybe
import Data.Either
import Data.Foldable (all, foldr)
import Data.Traversable (for, traverse)

import Data.Tuple
import Data.Tuple3

import Control.Apply
import Control.Bind ((<=<), join)
import Control.Monad (unless, replicateM)

import Control.Monad.Error
import Control.Monad.Error.Class

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Supply

-- |
-- Replace all top-level binders in a module with case expressions.
--
desugarCasesModule :: [Module] -> SupplyT (Either ErrorStack) [Module]
desugarCasesModule ms = for ms $ \(Module name ds exps) ->
  rethrow (\s -> (strMsg ("Error in module " ++ show name) :: ErrorStack) <> s) $
    Module name <$> (desugarCases <=< desugarAbs $ ds) <*> pure exps

desugarAbs :: [Declaration] -> SupplyT (Either ErrorStack) [Declaration]
desugarAbs = 
  case everywhereOnValuesM return replace return of
    Tuple3 f _ _ -> traverse f
  where
  replace :: Value -> SupplyT (Either ErrorStack) Value
  replace (Abs (Right binder) val) = do
    ident <- Ident <$> freshName
    return $ Abs (Left ident) $ Case [Var (Qualified Nothing ident)] [mkCaseAlternative [binder] Nothing val]
  replace other = return other

-- |
-- Replace all top-level binders with case expressions.
--
desugarCases :: [Declaration] -> SupplyT (Either ErrorStack) [Declaration]
desugarCases ds = do
  dss <- traverse toDecls $ groupBy inSameGroup $ ds
  desugarRest (join dss)
  where
    desugarRest :: [Declaration] -> SupplyT (Either ErrorStack) [Declaration]
    desugarRest (TypeInstanceDeclaration name constraints className tys ds : rest) =
      (:) <$> (TypeInstanceDeclaration name constraints className tys <$> desugarCases ds) <*> desugarRest rest
    desugarRest (ValueDeclaration name nameKind bs g val : rest) =
      case everywhereOnValuesTopDownM return go return of
        Tuple3 _ f _ -> (:) <$> (ValueDeclaration name nameKind bs g <$> f val) <*> desugarRest rest
      where
      go (Let ds val') = Let <$> desugarCases ds <*> pure val'
      go other = return other
    desugarRest (PositionedDeclaration pos d : ds) = do
      (d' : ds') <- desugarRest (d : ds)
      return (PositionedDeclaration pos d' : ds')
    desugarRest (d : ds) = (:) d <$> desugarRest ds
    desugarRest [] = pure []

inSameGroup :: Declaration -> Declaration -> Boolean
inSameGroup (ValueDeclaration ident1 _ _ _ _) (ValueDeclaration ident2 _ _ _ _) = ident1 == ident2
inSameGroup (PositionedDeclaration _ d1) d2 = inSameGroup d1 d2
inSameGroup d1 (PositionedDeclaration _ d2) = inSameGroup d1 d2
inSameGroup _ _ = false

toDecls :: [Declaration] -> SupplyT (Either ErrorStack) [Declaration]
toDecls [ValueDeclaration ident nameKind bs Nothing val] | all isVarBinder bs = do
  let args = map (\(VarBinder arg) -> arg) bs
      body = foldr (Abs <<< Left) val args
  return [ValueDeclaration ident nameKind [] Nothing body]
toDecls ds@(ValueDeclaration ident _ bs _ _ : _) = do
  let tuples = map toTuple ds
  unless (all (((==) (length bs)) <<< length <<< fst) tuples) $
    throwError $ mkErrorStack ("Argument list lengths differ in declaration " ++ show ident) Nothing
  caseDecl <- makeCaseDeclaration ident tuples
  return [caseDecl]
toDecls (PositionedDeclaration pos d : ds) = do
  (d' : ds') <- rethrowWithPosition pos $ toDecls (d : ds)
  return (PositionedDeclaration pos d' : ds')
toDecls ds = return ds

isVarBinder :: Binder -> Boolean
isVarBinder (VarBinder _) = true
isVarBinder _ = false

toTuple :: Declaration -> Tuple [Binder] (Tuple (Maybe Guard) Value)
toTuple (ValueDeclaration _ _ bs g val) = Tuple bs (Tuple g val)
toTuple (PositionedDeclaration _ d) = toTuple d
toTuple _ = theImpossibleHappened "Not a value declaration"

makeCaseDeclaration :: Ident -> [Tuple [Binder] (Tuple (Maybe Guard) Value)] -> SupplyT (Either ErrorStack) Declaration
makeCaseDeclaration ident alternatives = do
  let argPattern = length <<< fst <<< Unsafe.head $ alternatives
  args <- map Ident <$> replicateM argPattern freshName
  let
    vars = map (Var <<< Qualified Nothing) args
    binders = do (Tuple bs (Tuple g val)) <- alternatives
                 return $ mkCaseAlternative bs g val
    value = foldr (Abs <<< Left) (Case vars binders) args
  return $ ValueDeclaration ident Value [] Nothing value