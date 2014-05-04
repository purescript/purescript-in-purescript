-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Scope
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Utility functions for working with names in scope
--
-----------------------------------------------------------------------------

module Language.PureScript.Scope (
    usedNamesDecl,
    usedNamesValue,
    usedNamesBinder,
    usedNamesCaseAlternative,
    usedNamesDoNotationElement,
    unusedNames
  ) where

import Data.Tuple5
import Data.Array (nub)
import Data.Either
import Data.Maybe
import Data.List (List(..), (\\), enumFrom)

import Language.PureScript.Declarations
import Language.PureScript.Names

usedNames :: Tuple5 (Declaration -> [Ident]) (Value -> [Ident]) (Binder -> [Ident]) (CaseAlternative -> [Ident]) (DoNotationElement -> [Ident])
usedNames = everythingOnValues (++) f g h (const []) (const [])
  where
  f :: Declaration -> [Ident]
  f (ValueDeclaration name _ _ _ _) = [name]
  f _ = []

  g :: Value -> [Ident]
  g (Abs (Left arg) _) = [arg]
  g (Var (Qualified Nothing name)) = [name]
  g _ = []

  h :: Binder -> [Ident]
  h (VarBinder name) = [name]
  h _ = []

-- |
-- Gather all used names appearing inside a declaration
--
usedNamesDecl :: Declaration -> [Ident]
usedNamesDecl = case usedNames of
	Tuple5 f _ _ _ _ -> nub <<< f

-- |
-- Gather all used names appearing inside a value
--
usedNamesValue :: Value -> [Ident]
usedNamesValue = case usedNames of
	Tuple5 _ f _ _ _ -> nub <<< f

-- |
-- Gather all used names appearing inside a binder
--
usedNamesBinder :: Binder -> [Ident]
usedNamesBinder = case usedNames of
	Tuple5 _ _ f _ _ -> nub <<< f

-- |
-- Gather all used names appearing inside a case alternative
--
usedNamesCaseAlternative :: CaseAlternative -> [Ident]
usedNamesCaseAlternative = case usedNames of
	Tuple5 _ _ _ f _ -> nub <<< f

-- |
-- Gather all used names appearing inside a do notation element
--
usedNamesDoNotationElement :: DoNotationElement -> [Ident]
usedNamesDoNotationElement = case usedNames of
	Tuple5 _ _ _ _ f -> nub <<< f

-- |
-- Generate a set of names which are unused inside a value, of the form @_{n}@ for an integer @n@
--
unusedNames :: [Ident] -> List Ident
unusedNames allNames =
  let
    varNames = Ident <<< ((++) "_") <<< show <$> enumFrom 1
  in
    varNames \\ allNames