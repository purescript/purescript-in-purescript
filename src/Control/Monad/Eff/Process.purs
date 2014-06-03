module Control.Monad.Eff.Process where

import Control.Monad.Eff

foreign import data Process :: !

foreign import exit
  "function exit(code) {\
  \  return function() {\
  \    process.exit(code);\
  \  };\
  \}" :: forall eff. Number -> Eff (process :: Process | eff) {}