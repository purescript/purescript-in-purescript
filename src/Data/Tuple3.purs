-----------------------------------------------------------------------------
--
-- Module      :  Data.Tuple3
-- Copyright   :  (c) 2014 Phil Freeman
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | A tuple with three values
--
-----------------------------------------------------------------------------

module Data.Tuple3 where
	
data Tuple3 a b c = Tuple3 a b c

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show (Tuple3 a b c) = "Tuple3 (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ")"
  
instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  (==) (Tuple3 a1 b1 c1) (Tuple3 a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
  (/=) (Tuple3 a1 b1 c1) (Tuple3 a2 b2 c2) = (a1 /= a2) || (b1 /= b2) || (c1 /= c2)