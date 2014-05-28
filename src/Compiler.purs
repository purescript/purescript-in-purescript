-----------------------------------------------------------------------------
--
-- Module      :  Compiler
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | psc frontend to the PureScript library
--
-----------------------------------------------------------------------------

module Compiler where

import Debug.Trace

import Data.Maybe
import Data.Tuple
import Data.Tuple3
import Data.Array (concat, map)
import Data.Either

import Data.Traversable (for)

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Cont.Trans

import Node.Args

import Language.PureScript
import Language.PureScript.Declarations
import Language.PureScript.Options

import qualified Language.PureScript.Parser.Lexer as P
import qualified Language.PureScript.Parser.Common as P
import qualified Language.PureScript.Parser.Declarations as P

foreign import data FS :: !

foreign import data Process :: !

foreign import exit
  "function exit(code) {\
  \  return function() {\
  \    process.exit(code);\
  \  };\
  \}" :: forall eff. Number -> Eff (process :: Process | eff) {}

foreign import readFile
  "function readFile(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        require('fs').readFile(filename, 'utf8', function(err, data) {\
  \          if (err) {\
  \            fail(err)();\
  \          } else {\
  \            k(data)();\
  \          }\
  \        });\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff. String -> (String -> Eff (fs :: FS | eff) {}) -> (String -> Eff (fs :: FS | eff) {}) -> Eff (fs :: FS | eff) {}

type AppMonad eff = ErrorT String (ContT {} (Eff (fs :: FS | eff)))

runAppMonad :: forall eff a. AppMonad eff a -> (Either String a -> Eff (fs :: FS | eff) {}) -> Eff (fs :: FS | eff) {}
runAppMonad app = runContT (runErrorT app)

readFileCont :: forall eff. String -> AppMonad eff String
readFileCont filename = ErrorT $ ContT $ \k -> readFile filename (k <<< Right) (k <<< Left)

preludeFilename :: String
preludeFilename = "prelude/prelude.purs"

modulesFromText :: String -> Either String [Module]
modulesFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModules tokens
  
readInput :: forall eff. [String] -> AppMonad eff [Module]
readInput input = 
  concat <$> for input (\inputFile -> do
    text <- readFileCont inputFile
    case modulesFromText text of
      Left err -> throwError err
      Right ms -> return ms)

runCompiler :: forall eff. Options -> [String] -> Maybe String -> Maybe String -> Eff (trace :: Trace, fs :: FS, process :: Process | eff) {}
runCompiler opts input output externs = runAppMonad (readInput input) $ \modules ->
  case modules of
    Left err -> do
      trace err
      exit 1
    Right ms -> do
      case compile opts ms of
        Left err -> do
          trace err
          exit 1
        Right (Tuple3 js exts _) -> do
          case output of
            Just path -> trace "Not implemented" -- mkdirp path >> U.writeFile path js
            Nothing -> trace js
          case externs of
            Just path -> trace "Not implemented" -- mkdirp path >> U.writeFile path exts
            Nothing -> return {}
          exit 0

flag :: String -> String -> Args Boolean
flag shortForm longForm = maybe false (const true) <$> opt (flagOnly shortForm <|> flagOnly longForm)

useStdIn :: Args Boolean
useStdIn = flag "s" "stdin"

inputFiles :: Args [String]
inputFiles = many argOnly

outputFile :: Args (Maybe String)
outputFile = opt (flagArg "o" <|> flagArg "output")

externsFile :: Args (Maybe String)
externsFile = opt (flagArg "e" <|> flagArg "externs")

noTco :: Args Boolean
noTco = flagOpt "no-tco"

performRuntimeTypeChecks :: Args Boolean
performRuntimeTypeChecks = flagOpt "runtime-type-checks"

noPrelude :: Args Boolean
noPrelude = flagOpt "no-prelude"

noMagicDo :: Args Boolean
noMagicDo = flagOpt "no-magic-do"

runMain :: Args (Maybe String)
runMain = opt (flagArg "main")

noOpts :: Args Boolean
noOpts = flagOpt "no-opts"

browserNamespace :: Args String
browserNamespace = flagArg "browser-namespace" <|> pure "PS"

dceModules :: Args [String]
dceModules = many (flagArg "m" <|> flagArg "module")

codeGenModules :: Args [String]
codeGenModules = many (flagArg "codegen")

verboseErrors :: Args Boolean
verboseErrors = flag "v" "verbose-errors"

options :: Args Options
options = mkOptions <$> noPrelude 
                    <*> noTco 
                    <*> performRuntimeTypeChecks 
                    <*> noMagicDo 
                    <*> runMain 
                    <*> noOpts 
                    <*> (Just <$> browserNamespace) 
                    <*> dceModules 
                    <*> codeGenModules 
                    <*> verboseErrors

inputFilesAndPrelude :: Args [String]
inputFilesAndPrelude = combine <$> noPrelude <*> inputFiles
  where
  combine true  input = input
  combine false input = preludeFilename : input

term :: Args (Eff (trace :: Trace, fs :: FS, process :: Process) {})
term = runCompiler <$> options <*> inputFilesAndPrelude <*> outputFile <*> externsFile

main = catchException (\err -> print err) $ readArgs' term