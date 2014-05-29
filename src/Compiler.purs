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
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Exception

import Control.Apply
import Control.Monad.Trans
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
  \        try {\
  \          return k(require('fs').readFileSync(filename, 'utf8'));\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> (String -> r) -> (String -> r) -> Eff (fs :: FS | eff) r
  
foreign import writeFile
  "function writeFile(filename) {\
  \  return function(data) {\
  \    return function(k) {\
  \      return function(fail) {\
  \        return function() {\
  \          try {\
  \            return k(require('fs').writeFileSync(filename, data, 'utf8'));\
  \          } catch(err) {\
  \            return fail(err);\
  \          }\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> String -> ({} -> r) -> (String -> r) -> Eff (fs :: FS | eff) r

type AppEffects = (fs :: FS, trace :: Trace, process :: Process)

type AppMonad = ErrorT String (Eff AppEffects)

runAppMonad :: forall a. AppMonad a -> Eff AppEffects {}
runAppMonad app = do
  result <- runErrorT app
  case result of
    Left err -> do
      trace err
      exit 1
    Right _ -> exit 0

readFileApp :: String -> AppMonad String
readFileApp filename = ErrorT $ readFile filename Right Left

writeFileApp :: String -> String -> AppMonad {}
writeFileApp filename text = ErrorT $ writeFile filename text Right Left

eitherApp :: forall a. Either String a -> AppMonad a
eitherApp e = ErrorT (return e)

preludeFilename :: String
preludeFilename = "prelude/prelude.purs"

modulesFromText :: String -> Either String [Module]
modulesFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModules tokens
  
readInput :: forall eff. [String] -> AppMonad [Module]
readInput input = 
  concat <$> for input (\inputFile -> do
    text <- readFileApp inputFile
    case modulesFromText text of
      Left err -> throwError err
      Right ms -> return ms)

runCompiler :: forall eff. Options -> [String] -> Maybe String -> Maybe String -> Eff AppEffects {}
runCompiler opts@(Options optso) input output externs = runAppMonad do
  modules <- readInput allInputFiles
  Tuple3 js exts _ <- eitherApp $ compile opts modules
  case output of
    Nothing -> lift $ trace js
    Just path -> writeFileApp path js
  for externs $ \path -> writeFileApp path exts
  return {}
  where
  allInputFiles :: [String]
  allInputFiles | optso.noPrelude = input
  allInputFiles = preludeFilename : input

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

term :: Args (Eff AppEffects {})
term = runCompiler <$> options <*> inputFiles <*> outputFile <*> externsFile

main = do
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return {}

