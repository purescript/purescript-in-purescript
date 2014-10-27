-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Keywords
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Various reserved keywords and operator names
--
-----------------------------------------------------------------------------

module Language.PureScript.Keywords where

import Data.Char
import Data.String (toCharArray)

-- |
-- A list of purescript reserved identifiers
--
reservedPsNames :: [String]
reservedPsNames =
  [ "data"
  , "type"
  , "foreign"
  , "import"
  , "infixl"
  , "infixr"
  , "infix"
  , "class"
  , "instance"
  , "module"
  , "case"
  , "of"
  , "if"
  , "then"
  , "else"
  , "do"
  , "let"
  , "true"
  , "false"
  , "in"
  , "where" ]

-- |
-- A list of additionally reserved identifiers for types
--
reservedTypeNames :: [String]
reservedTypeNames =
  [ "forall"
  , "where" ]

opCharsString :: String
opCharsString = ":!#$%&*+./<=>?@\\^|-~"

-- |
-- The characters allowed for use in operators
--
opChars :: [Char]
opChars = toCharArray opCharsString

-- |
-- A list of reserved operators
--
reservedOpNames :: [String]
reservedOpNames =
  [ "=>"
  , "->"
  , "="
  , "."
  , "\\" ]
