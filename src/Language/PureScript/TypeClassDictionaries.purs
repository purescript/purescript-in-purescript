module Language.PureScript.TypeClassDictionaries where

import Data.Maybe
import Data.Tuple

import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Show

-- |
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope = TypeClassDictionaryInScope
  {
    -- |
    -- The identifier with which the dictionary can be accessed at runtime
    --
      tcdName :: Qualified Ident
    -- |
    -- The name of the type class to which this type class instance applies
    --
    , tcdClassName :: Qualified ProperName
    -- |
    -- The types to which this type class instance applies
    --
    , tcdInstanceTypes :: [Type]
    -- |
    -- Type class dependencies which must be satisfied to construct this dictionary
    --
    , tcdDependencies :: Maybe [Tuple (Qualified ProperName) [Type]]
    -- |
    -- The type of this dictionary
    --
    , tcdType :: TypeClassDictionaryType
    }

instance showTCDIS :: Show TypeClassDictionaryInScope where
  show = defaultShow

-- |
-- The type of a type class dictionary
--
data TypeClassDictionaryType
  -- |
  -- A regular type class dictionary
  --
  = TCDRegular
  -- |
  -- A type class dictionary which is an alias for an imported dictionary from another module
  --
  | TCDAlias (Qualified Ident)

instance showTCDT :: Show TypeClassDictionaryType where
  show = defaultShow

instance eqTCDT :: Eq TypeClassDictionaryType where
  (==) TCDRegular    TCDRegular = true
  (==) (TCDAlias q1) (TCDAlias q2) = true
  (==) _ _ = false
  (/=) x y = not (x == y)
