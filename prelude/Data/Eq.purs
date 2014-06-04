module Data.Eq where

data Ref a = Ref a

liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b
liftRef f (Ref x) (Ref y) = f x y

instance eqRef :: Eq (Ref a) where
  (==) = liftRef refEq
  (/=) = liftRef refIneq
