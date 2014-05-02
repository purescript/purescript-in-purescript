module Data.Tuple5 where
	
data Tuple5 a b c d e = Tuple5 a b c d e

instance showTuple5 :: (Show a, Show b, Show c, Show d, Show e) => Show (Tuple5 a b c d e) where
  show (Tuple5 a b c d e) = "Tuple5 (" ++ show a ++ ") (" ++ show b ++ ") (" ++ show c ++ ") (" ++ show d ++ ") (" ++ show e ++ ")"
  
instance eqTuple5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (Tuple5 a b c d e) where
  (==) (Tuple5 a1 b1 c1 d1 e1) (Tuple5 a2 b2 c2 d2 e2) = (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2) && (e1 == e2)
  (/=) x y = not (x == y)