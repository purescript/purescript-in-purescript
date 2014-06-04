module Debug.Trace where

import Control.Monad.Eff

foreign import data Trace :: !

foreign import trace "function trace(s) {\
                     \  return function() {\
                     \    console.log(s);\
                     \    return {};\
                     \  };\
                     \}" :: forall r. String -> Eff (trace :: Trace | r) {}

print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) {}
print o = trace (show o)
