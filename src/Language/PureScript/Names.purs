module Language.PureScript.Names where

import Data.Array (map)
import Data.Maybe
import Data.Tuple
import Data.String (joinWith, split)
  
-- |
-- Names for value identifiers
--
data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident String
  -- |
  -- A symbolic name for an infix operator
  --
  | Op String
  
runIdent :: Ident -> String
runIdent (Ident i) = i
runIdent (Op op) = op

instance showIdent :: Show Ident where
  show (Ident i) = i
  show (Op op) = "(" ++ op ++ ")"
  
instance eqIdent :: Eq Ident where
  (==) (Ident s1) (Ident s2) = s1 == s2
  (==) (Op s1)    (Op s2)    = s1 == s2
  (==) _          _          = false
  (/=) i1         i2         = not (i1 == i2)

instance ordIdent :: Ord Ident where
  compare (Ident i1) (Ident i2) = compare i1 i2
  compare (Ident _)  _          = LT
  compare (Op op1)   (Op op2)   = compare op1 op2
  compare (Op op1)   _          = GT

-- |
-- Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
--
data ProperName = ProperName String

runProperName :: ProperName -> String
runProperName (ProperName s) = s

instance showProperName :: Show ProperName where
  show = runProperName

instance eqProperName :: Eq ProperName where
  (==) (ProperName s1) (ProperName s2) = s1 == s2
  (/=) (ProperName s1) (ProperName s2) = s1 /= s2

instance ordProperName :: Ord ProperName where
  compare (ProperName s1) (ProperName s2) = compare s1 s2

-- |
-- Module names
--
data ModuleName = ModuleName [ProperName] 

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = joinWith "." (runProperName `map` pns)

instance showModuleName :: Show ModuleName where
  show = runModuleName

instance eqModuleName :: Eq ModuleName where
  (==) (ModuleName s1) (ModuleName s2) = s1 == s2
  (/=) (ModuleName s1) (ModuleName s2) = s1 /= s2

instance ordModuleName :: Ord ModuleName where
  compare (ModuleName s1) (ModuleName s2) = compare s1 s2

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName <<< map ProperName <<< split "."

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

instance showQualified :: (Show a) => Show (Qualified a) where
  show (Qualified Nothing a) = show a
  show (Qualified (Just name) a) = show name ++ "." ++ show a
  
instance eqQualified :: (Eq a) => Eq (Qualified a) where
  (==) (Qualified m1 a1) (Qualified m2 a2) = m1 == m2 && a1 == a2
  (/=) q1 q2 = not (q1 == q2)
  
instance ordQualified :: (Ord a) => Ord (Qualified a) where
  compare (Qualified m1 a1) (Qualified m2 a2) = case compare m1 m2 of
    EQ -> compare a1 a2
    other -> other

-- |
-- Provide a default module name, if a name is unqualified
--
qualify :: forall a. ModuleName -> Qualified a -> Tuple ModuleName a
qualify m (Qualified Nothing a) = Tuple m a
qualify _ (Qualified (Just m) a) = Tuple m a
