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
import Control.Monad.Error.Proxy

import Language.PureScript.Pos
import Language.PureScript.Declarations
import Language.PureScript.Pretty.Values
import Language.PureScript.Pretty.Types
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
    message :: String
    -- |
    -- The value where the error occurred
    --
  , value :: Maybe ErrorSource
    -- |
    -- Optional source position information
    --
  , position :: Maybe SourcePos
  }
  
mkCompileError :: String -> Maybe ErrorSource -> Maybe SourcePos -> CompileError
mkCompileError message value position = 
  CompileError { message: message
	             , value: value
							 , position: position
							 }
  
instance showCompileError :: Show CompileError where
  show (CompileError o) = "CompileError {" ++
    "message: " ++  show o.message ++ ", " ++
    "value: " ++    show o.value ++ ", " ++
    "position: " ++ show o.position ++ " " ++
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
  
unifyError :: WithErrorType ErrorStack
unifyError = WithErrorType

prettyPrintErrorStack :: Boolean -> ErrorStack -> String
prettyPrintErrorStack printFullStack (ErrorStack es) =
  case mconcat $ map (\(CompileError o) -> Last o.position) es of
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
          _ -> showError (Unsafe.head es') ++ "\n" ++ showError (Unsafe.last es')

stringifyErrorStack :: forall a. Boolean -> Either ErrorStack a -> Either String a
stringifyErrorStack printFullStack = either (Left <<< prettyPrintErrorStack printFullStack) Right

isErrorNonEmpty :: CompileError -> Boolean
isErrorNonEmpty (CompileError { message = "" }) = false
isErrorNonEmpty _ = true

showError :: CompileError -> String
showError (CompileError { message = msg, value = Nothing }) = msg
showError (CompileError { message = msg, value = Just (ValueError val) }) = "Error in value " ++ prettyPrintValue val ++ ":\n" ++ msg
showError (CompileError { message = msg, value = Just (TypeError ty) }) = "Error in type " ++ prettyPrintType ty ++ ":\n" ++ msg

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

-- |
-- This is a hack to emulate the semantics of Haskell's "error" function.
-- It can only be used with types whose runtime representation is an Object.
--
foreign import error 
  "function error(msg) {\
  \  var explode = function() {\
  \    this.__defineGetter__('ctor', function() {\
  \      throw new Error(msg);\
  \    });\
  \  };\
  \  return new explode();\
  \}" :: forall a. a
