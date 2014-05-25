module Control.Monad.Error.Proxy where

--
-- A proxy for the type variable a
--
data WithErrorType e = WithErrorType

withErrorType :: forall e. WithErrorType e -> e -> e
withErrorType _ e = e