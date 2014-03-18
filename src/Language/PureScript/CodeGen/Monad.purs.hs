module Language.PureScript.CodeGen.Monad where

import Prelude
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.State.Class
import Control.Applicative

-- |
-- Code generation monad data type
--
data Gen a = Gen (State [String] a)

unGen :: forall a. Gen a -> State [String] a
unGen (Gen x) = x

instance monadGen :: Monad Gen where
  return = Gen <<< return
  (>>=) (Gen x) f = Gen (x >>= unGen <<< f)

-- TODO: this should be `MonadState [String] Gen`
instance monadStateGen :: MonadState [a] Gen where
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
