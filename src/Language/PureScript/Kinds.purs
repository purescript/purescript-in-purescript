module Language.PureScript.Kinds where

import Control.Monad.Unify (Unknown())

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
  show (KUnknown u) = "KUnknown " ++ show u
  show Star = "Star"
  show Bang = "Bang"
  show (Row k) = "Row " ++ show k
  show (FunKind x y) = "FunKind (" ++ show x ++ ") (" ++ show y ++ ")"

instance eqKind :: Eq Kind where
  (==) (KUnknown u1) (KUnknown u2) = u1 == u2
  (==) Star Star = true
  (==) Bang Bang = true
  (==) (Row k1) (Row k2) = k1 == k2
  (==) (FunKind x1 y1) (FunKind x2 y2) = x1 == y1 && x2 == y2
  (==) _ _ = false
  (/=) x y = not (x == y)

everywhereOnKinds :: (Kind -> Kind) -> Kind -> Kind
everywhereOnKinds f = go
  where
  go (Row k1) = f (Row (go k1))
  go (FunKind k1 k2) = f (FunKind (go k1) (go k2))
  go other = f other

everythingOnKinds :: forall r. (r -> r -> r) -> (Kind -> r) -> Kind -> r
everythingOnKinds (<>) f = go
  where
  go k@(Row k1) = f k <> go k1
  go k@(FunKind k1 k2) = f k <> go k1 <> go k2
  go other = f other
