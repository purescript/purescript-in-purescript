-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Error
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Errors where

import Prelude.Unsafe (unsafeIndex)
import Data.Array
import qualified Data.Array.Unsafe as Unsafe
import Data.String (joinWith)
import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Monoid.Last
import Data.Foldable (mconcat)

import Control.Monad.Error
import Control.Monad.Error.Class

import Language.PureScript.Declarations
{- import Language.PureScript.Pretty -}
import Language.PureScript.Types

-- |
-- Type for sources of type checking errors
--
data ErrorSource
  -- |
  -- An error which originated at a Value
  --
  = ValueError Value
  -- |
  -- An error which originated at a Type
  --
  | TypeError Type
  
instance showErrorSource :: Show ErrorSource where
  show (ValueError v) = "ValueError (" ++ show v ++ ")"
  show (TypeError t) = "TypeError (" ++ show t ++ ")"

-- |
-- Compilation errors
--
data CompileError = CompileError {
    -- |
    -- Error message
    --
    compileErrorMessage :: String
    -- |
    -- The value where the error occurred
    --
  , compileErrorValue :: Maybe ErrorSource
    -- |
    -- Optional source position information
    --
  , compileErrorPosition :: Maybe SourcePos
  }
  
mkCompileError :: String -> Maybe ErrorSource -> Maybe SourcePos -> CompileError
mkCompileError compileErrorMessage compileErrorValue compileErrorPosition = 
  CompileError { compileErrorMessage: compileErrorMessage
	             , compileErrorValue: compileErrorValue
							 , compileErrorPosition: compileErrorPosition
							 }
  
instance showCompileError :: Show CompileError where
  show (CompileError o) = "CompileError {" ++
    "compileErrorMessage: " ++  show o.compileErrorMessage ++
    "compileErrorValue: " ++    show o.compileErrorValue ++
    "compileErrorPosition: " ++ show o.compileErrorPosition ++
	  "}" 

-- |
-- A stack trace for an error
--
data ErrorStack = ErrorStack [CompileError] 

runErrorStack :: ErrorStack -> [CompileError] 
runErrorStack (ErrorStack es) = es

instance showErrorStack :: Show ErrorStack where
  show (ErrorStack es) = "ErrorStack (" ++ show es ++ ")"
  
instance semigroupErrorStack :: Semigroup ErrorStack where
  (<>) (ErrorStack es1) (ErrorStack es2) = ErrorStack (es1 <> es2)
  
instance monoidErrorStack :: Monoid ErrorStack where
  mempty = ErrorStack []

instance errorErrorStack :: Error ErrorStack where
  strMsg s = ErrorStack [mkCompileError s Nothing Nothing]
  noMsg = ErrorStack []

prettyPrintErrorStack :: Boolean -> ErrorStack -> String
prettyPrintErrorStack printFullStack (ErrorStack es) =
  case mconcat $ map (\(CompileError o) -> Last o.compileErrorPosition) es of
    Last (Just sourcePos) -> "Error at " ++ show sourcePos ++ ": \n" ++ prettyPrintErrorStack'
    _ -> prettyPrintErrorStack'
  where
	  
    prettyPrintErrorStack' :: String
    prettyPrintErrorStack' = 
		  if printFullStack 
	    then
		  	joinWith "\n" (map showError (filter isErrorNonEmpty es))
      else
		  	let
          es' = filter isErrorNonEmpty es
        in case length es' of
          1 -> showError (Unsafe.head es')
          _ -> showError (Unsafe.head es') ++ "\n" ++ showError (unsafeLast es')

-- TODO: move this to Data.Array.Unsafe					
unsafeLast :: forall a. [a] -> a
unsafeLast xs = unsafeIndex xs (length xs - 1)

stringifyErrorStack :: forall a. Boolean -> Either ErrorStack a -> Either String a
stringifyErrorStack printFullStack = either (Left <<< prettyPrintErrorStack printFullStack) Right

isErrorNonEmpty :: CompileError -> Boolean
isErrorNonEmpty (CompileError { compileErrorMessage = "" }) = false
isErrorNonEmpty _ = true

showError :: CompileError -> String
showError (CompileError { compileErrorMessage = msg, compileErrorValue = Nothing }) = msg
showError (CompileError { compileErrorMessage = msg, compileErrorValue = Just (ValueError val) }) = "Error in value " ++ {- prettyPrintValue -} show val ++ ":\n" ++ msg
showError (CompileError { compileErrorMessage = msg, compileErrorValue = Just (TypeError ty) }) = "Error in type " ++ {- prettyPrintType -} show ty ++ ":\n" ++ msg

mkErrorStack :: String -> Maybe ErrorSource -> ErrorStack
mkErrorStack msg t = ErrorStack [mkCompileError msg t Nothing]

positionError :: SourcePos -> ErrorStack
positionError pos = ErrorStack [mkCompileError "" Nothing (Just pos)]

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: forall e m a. (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

-- |
-- Rethrow an error with source position information
--
rethrowWithPosition :: forall m a. (MonadError ErrorStack m) => SourcePos -> m a -> m a
rethrowWithPosition pos = rethrow ((<>) (positionError pos))

-- |
-- Throw a runtime error for a situation where something unexpected (and 
-- supposedly impossible) happened
--
foreign import theImpossibleHappened
  "function theImpossibleHappened(msg) {\
  \  throw new Error(msg);\
  \}" :: forall a. String -> a 
