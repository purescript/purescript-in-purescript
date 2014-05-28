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
import Data.Either

import Data.Traversable (for)

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State.Class

import Node.Args

import Language.PureScript
import Language.PureScript.Declarations
import Language.PureScript.Options

import qualified Language.PureScript.Parser.Lexer as P
import qualified Language.PureScript.Parser.Common as P
import qualified Language.PureScript.Parser.Declarations as P

foreign import data FS :: # ! -> !

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
  \            fail(err);\
  \          } else {\
  \            k(data);\
  \          }\
  \        });\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff rest. String -> (String -> Eff eff {}) -> (String -> Eff eff {}) -> Eff (fs :: FS eff | rest) {}

preludeFilename :: String
preludeFilename = "prelude/prelude.purs"

modulesFromText :: String -> Either String [Module]
modulesFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModules tokens

runCompiler :: Options -> [String] -> Maybe String -> Maybe String -> Eff (trace :: Trace) {}
runCompiler _ _ _ _ = trace "Not implemented"

{-
readInput :: [String] -> Eff eff (Either ParseError [Tuple String Module])
readInput input = 
  collect <$> for input $ \inputFile -> do
    text <- readFile inputFile
    let modules = modulesFromText text
    return $ (Tuple inputFile modules)
  where
  collect :: [(FilePath, Either ParseError [P.Module])] -> Either ParseError [(FilePath, P.Module)]
  collect = fmap concat . sequence . map (\(fp, e) -> fmap (map ((,) fp)) e)

compile :: Options -> [String] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile opts input output externs = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.print err
      exitFailure
    Right ms -> do
      case P.compile opts (map snd ms) of
        Left err -> do
          print err
          exit 1
        Right (Tuple3 js exts _) -> do
          case output of
            Just path -> mkdirp path >> U.writeFile path js
            Nothing -> U.putStrLn js
          case externs of
            Just path -> mkdirp path >> U.writeFile path exts
            Nothing -> return {}
          exit 0

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing true . takeDirectory
-}

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
inputFilesAndPrelude = combine <$> (not <$> noPrelude) <*> inputFiles
  where
  combine true  input = preludeFilename : input
  combine false input = input

term :: Args (Eff (trace :: Trace) {})
term = runCompiler <$> options <*> inputFilesAndPrelude <*> outputFile <*> externsFile

main = catchException (\err -> print err) $ readArgs' term