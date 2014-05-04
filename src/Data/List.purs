-----------------------------------------------------------------------------
--
-- Module      :  Data.List
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Enumerators for singly-linked lists
--
-----------------------------------------------------------------------------

module Data.List where
	
data List a = Nil | Cons a ({} -> List a)

instance functorList :: Functor List where
	(<$>) _ Nil = Nil
	(<$>) f (Cons h t) = Cons (f h) $ \_ -> f <$> t {}
	
filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter f (Cons h t) | f h = Cons h $ \_ -> filter f (t {})
filter f (Cons h t) = filter f (t {})

enumFrom :: Number -> List Number
enumFrom n = Cons n $ \_ -> enumFrom (n + 1)

fromArray :: forall a. [a] -> List a
fromArray [] = Nil
fromArray (h : t) = Cons h $ \_ -> fromArray t

toArray :: forall a. List a -> [a]
toArray Nil = []
toArray (Cons h t) = h : toArray (t {})

(\\) :: forall a. (Eq a) => List a -> [a] -> List a
(\\) l arr = filter (\a -> not (a `elem` arr)) l
  where
  elem :: forall a. (Eq a) => a -> [a] -> Boolean
  elem _ [] = false
  elem a (h : _) | a == h = true
  elem a (_ : t) = elem a t
	
take :: forall a. Number -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take n (Cons h t) = Cons h $ \_ -> take (n - 1) (t {}) 