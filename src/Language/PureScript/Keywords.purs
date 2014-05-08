module Language.PureScript.Keywords where

import Data.String (split)

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
  
-- |
-- The characters allowed for use in operators
--
opChars :: [String]
opChars = split "" ":!#$%&*+./<=>?@\\^|-~"

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