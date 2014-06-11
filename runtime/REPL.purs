module REPL where

import Debug.Trace

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe

class EvalPrint a where
  evalPrint :: a -> Eff (trace :: Trace) {}

instance evalPrintEff :: (Show a) => EvalPrint (Eff eff a) where
  evalPrint e = do
    a <- unsafeInterleaveEff e
    print a

instance evalPrintOther :: (Show a) => EvalPrint a where
  evalPrint = print
