module Language.PureScript.CodeGen.Monad where

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.State.Class

-- |
-- Code generation monad data type
--
data Gen a = Gen (State [String] a)

unGen :: forall a. Gen a -> State [String] a
unGen (Gen x) = x

instance functorGen :: Functor Gen where
  (<$>) = liftA1
  
instance applyGen :: Apply Gen where
  (<*>) = ap

instance applicativeGen :: Applicative Gen where
  pure = Gen <<< return

instance bindGen :: Bind Gen where
  (>>=) (Gen x) f = Gen (x >>= unGen <<< f)
  
instance monadGen :: Monad Gen

instance monadStateGen :: MonadState [String] Gen where
  state = Gen <<< state

-- |
-- Run a computation in the code generation monad
--
runGen :: forall a. [String] -> Gen a -> a
runGen names = flip evalState names <<< unGen

-- |
-- Generate a fresh name
--
fresh :: Gen String
fresh = do
  (s:ss) <- get
  put ss
  return s
