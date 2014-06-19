-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeDeclarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level type declarations with
-- type annotations on the corresponding expression.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.TypeDeclarations (
    desugarTypeDeclarations,
    desugarTypeDeclarationsModule
  ) where

import Data.Maybe
import Data.Either
import Data.Traversable (for)
import Data.Tuple3

import Control.Apply

import Control.Monad.Error
import Control.Monad.Error.Class

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all top level type declarations in a module with type annotations
--
desugarTypeDeclarationsModule :: [Module] -> Either ErrorStack [Module]
desugarTypeDeclarationsModule ms = for ms $ \(Module name ds exps) ->
  rethrow (\e -> strMsg ("Error in module " ++ show name) <> (e :: ErrorStack)) $
    Module name <$> desugarTypeDeclarations ds <*> pure exps

-- |
-- Replace all top level type declarations with type annotations
--
desugarTypeDeclarations :: [Declaration] -> Either ErrorStack [Declaration]
desugarTypeDeclarations (PositionedDeclaration pos d : ds) = do
  (d' : ds') <- rethrowWithPosition pos $ desugarTypeDeclarations (d : ds)
  return (PositionedDeclaration pos d' : ds')
desugarTypeDeclarations (TypeDeclaration name ty : d : rest) = do
  (Tuple3 _ nameKind val) <- fromValueDeclaration d
  desugarTypeDeclarations (ValueDeclaration name nameKind [] Nothing (TypedValue true val ty) : rest)
  where
  fromValueDeclaration :: Declaration -> Either ErrorStack (Tuple3 Ident NameKind Value)
  fromValueDeclaration (ValueDeclaration name' nameKind [] Nothing val) | name == name' = return (Tuple3 name' nameKind val)
  fromValueDeclaration (PositionedDeclaration pos d') = do
    (Tuple3 ident nameKind val) <- rethrowWithPosition pos $ fromValueDeclaration d'
    return (Tuple3 ident nameKind (PositionedValue pos val))
  fromValueDeclaration _ = throwError $ mkErrorStack ("Orphan type declaration for " ++ show name) Nothing
desugarTypeDeclarations (TypeDeclaration name _ : []) = throwError $ mkErrorStack ("Orphan type declaration for " ++ show name) Nothing
desugarTypeDeclarations (ValueDeclaration name nameKind bs g val : rest) = do
  (:) <$> (ValueDeclaration name nameKind bs g <$> f val) <*> desugarTypeDeclarations rest
  where
  f = (everywhereOnValuesTopDownM return go return).values
  go (Let ds val') = Let <$> desugarTypeDeclarations ds <*> pure val'
  go other = return other
desugarTypeDeclarations (d:ds) = (:) d <$> desugarTypeDeclarations ds
desugarTypeDeclarations [] = return []
