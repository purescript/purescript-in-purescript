module Language.PureScript.Names where

import Prelude
import Data.Array (map)
import Data.Maybe
import Data.Tuple
import Data.String
import Data.Generics
  
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
  -- |
  -- An escaped name
  --
  | Escaped String

instance genericIdent :: Generic Ident where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Names.Ident", args: [] }  
  term (Ident s) = TmCon { con: "Language.PureScript.Names.Ident", values: [term s] }
  term (Op s) = TmCon { con: "Language.PureScript.Names.Op", values: [term s] }
  term (Escaped s) = TmCon { con: "Language.PureScript.Names.Escaped", values: [term s] }
  unTerm (TmCon { con = "Language.PureScript.Names.Ident", values = [t] }) = Ident <$> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Names.Op", values = [t] }) = Op <$> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Names.Escaped", values = [t] }) = Escaped <$> unTerm t
  unTerm _ = Nothing

instance showIdent :: Show Ident where
  show = gshow

instance eqIdent :: Eq Ident where
  (==) (Ident s1)    (Ident s2)   = s1 == s2
  (==) (Op s1)       (Op s2)      = s1 == s2
  (==) (Escaped s1)  (Escaped s2) = s1 == s2
  (==) (Ident s1)    (Escaped s2) = s1 == s2
  (==) (Escaped s1)  (Ident s2)   = s1 == s2
  (==) _             _            = false
  (/=) i1            i2           = not (i1 == i2)

--instance ordIdent :: Ord Ident where
--  compare i1 i2 = compare (show i1) (show i2)

-- |
-- Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
--
data ProperName = ProperName String

runProperName :: ProperName -> String
runProperName (ProperName s) = s

instance genericProperName :: Generic ProperName where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Names.ProperName", args: [] }
  term (ProperName s) = TmCon { con: "Language.PureScript.Names.ProperName", values : [term s] }
  unTerm (TmCon { con = "Language.PureScript.Names.ProperName", values = [t] }) = ProperName <$> unTerm t
  unTerm _ = Nothing

instance showProperName :: Show ProperName where
  show = runProperName

instance eqProperName :: Eq ProperName where
  (==) (ProperName s1) (ProperName s2) = s1 == s2
  (/=) (ProperName s1) (ProperName s2) = s1 /= s2

--instance ordProperName :: Ord ProperName where
--  compare (ProperName s1) (ProperName s2) = compare s1 s2

-- |
-- Module names
--
data ModuleName = ModuleName [ProperName] 

instance genericModuleName :: Generic ModuleName where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Names.ModuleName", args: [] }
  term (ModuleName pns) = TmCon { con: "Language.PureScript.Names.ModuleName", values : [term pns] }
  unTerm (TmCon { con = "Language.PureScript.Names.ModuleName", values = [t] }) = ModuleName <$> unTerm t
  unTerm _ = Nothing

instance showModuleName :: Show ModuleName where
  show = runModuleName

instance eqModuleName :: Eq ModuleName where
  (==) (ModuleName s1) (ModuleName s2) = s1 == s2
  (/=) (ModuleName s1) (ModuleName s2) = s1 /= s2

--instance ordModuleName :: Ord ModuleName where
--  compare (ModuleName s1) (ModuleName s2) = compare s1 s2

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = joinWith "." (runProperName `map` pns)

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName <<< map ProperName <<< split "."

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

qualifiedProxy :: forall a. Proxy (Qualified a) -> Proxy a
qualifiedProxy _ = Proxy

instance genericQualified :: (Generic a) => Generic (Qualified a) where
  typeOf p = TyCon { tyCon: "Language.PureScript.Names.Qualified", args: [typeOf (qualifiedProxy p)]}
  term (Qualified mn a) = TmCon { con: "Language.PureScript.Names.Qualified", values : [term mn, term a] }
  unTerm (TmCon { con = "Language.PureScript.Names.Qualified", values = [mn, a] }) = Qualified <$> unTerm mn <*> unTerm a
  unTerm _ = Nothing

instance eqQualified :: (Eq a) => Eq (Qualified a) where
  (==) (Qualified m1 a1) (Qualified m2 a2) = m1 == m2 && a1 == a2
  (/=) q1 q2 = not (q1 == q2)

instance showQualified :: (Show a) => Show (Qualified a) where
  show (Qualified Nothing a) = show a
  show (Qualified (Just name) a) = show name ++ "." ++ show a

-- |
-- Provide a default module name, if a name is unqualified
--
qualify :: forall a. ModuleName -> Qualified a -> Tuple ModuleName a
qualify m (Qualified Nothing a) = Tuple m a
qualify _ (Qualified (Just m) a) = Tuple m a
