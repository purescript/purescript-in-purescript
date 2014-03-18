module Language.PureScript.Kinds where

import Prelude
import Control.Monad.Unify (Unknown())
import Data.Generics
import Data.Maybe

-- |
-- The data type of kinds
--
data Kind
  -- |
  -- Unification variable of type Kind
  --
  = KUnknown Unknown
  -- |
  -- The kind of types
  --
  | Star
  -- |
  -- The kind of effects
  --
  | Bang
  -- |
  -- Kinds for labelled, unordered rows without duplicates
  --
  | Row Kind
  -- |
  -- Function kinds
  --
  | FunKind Kind Kind
  
instance showKind :: Show Kind where
  show = gshow

instance eqKind :: Eq Kind where
  (==) = geq
  (/=) x y = not (geq x y)

instance genericKind :: Generic Kind where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Kinds.Kind", args: [] }
  term (KUnknown u)    = TmCon { con: "Language.PureScript.Kinds.KUnknown" , values: [term u] }
  term Star            = TmCon { con: "Language.PureScript.Kinds.Star"     , values: [] }
  term Bang            = TmCon { con: "Language.PureScript.Kinds.Bang"     , values: [] }
  term (Row k)         = TmCon { con: "Language.PureScript.Kinds.Row"      , values: [term k] }
  term (FunKind k1 k2) = TmCon { con: "Language.PureScript.Kinds.FunKind"  , values: [term k1, term k2] }
  unTerm (TmCon { con = "Language.PureScript.Kinds.KUnknown" , values = [u]      }) = KUnknown <$> unTerm u
  unTerm (TmCon { con = "Language.PureScript.Kinds.Star"                         }) = Just Star
  unTerm (TmCon { con = "Language.PureScript.Kinds.Bang"                         }) = Just Bang
  unTerm (TmCon { con = "Language.PureScript.Kinds.Row"      , values = [k]      }) = Row <$> unTerm k
  unTerm (TmCon { con = "Language.PureScript.Kinds.FunKind"  , values = [k1, k2] }) = FunKind <$> unTerm k1 <*> unTerm k2
  unTerm _ = Nothing
