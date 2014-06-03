-----------------------------------------------------------------------------
--
-- Module      :  Make
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | psc-make frontend to the PureScript library
--
-----------------------------------------------------------------------------

module Make where

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
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS

import Control.Apply
import Control.Monad.Application
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

instance monadMakeApp :: MonadMake Application where
  getTimestamp path = do
    exists <- doesFileExistApplication path
    case exists of
      true -> Just <$> getModificationTimeApplication path
      false -> return Nothing
  readTextFile path = do
    effApplication $ trace $ "Reading " ++ path
    readFileApplication path
  writeTextFile path text = do
    mkdirpApplication (dirname path)
    effApplication $ trace $ "Writing " ++ path
    writeFileApplication path text
  liftError = eitherApplication
  progress msg = effApplication $ trace msg

preludeFilename :: String
preludeFilename = "prelude/prelude.purs"

moduleFromText :: String -> Either String Module
moduleFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModule tokens
  
readInput :: forall eff. [String] -> Application [Tuple String Module]
readInput input = 
  for input (\inputFile -> do
    text <- readFileApplication inputFile
    case moduleFromText text of
      Left err -> throwError err
      Right m -> return (Tuple inputFile m))

runCompiler :: forall eff. String -> Options -> [String] -> Eff (fs :: FS, trace :: Trace, process :: Process) {}
runCompiler outputDir opts@(Options optso) input = runApplication do
  modules <- readInput allInputFiles
  make outputDir opts modules
  return {}
  where
  allInputFiles :: [String]
  allInputFiles | optso.noPrelude = input
  allInputFiles = preludeFilename : input

flag :: String -> String -> Args Boolean
flag shortForm longForm = maybe false (const true) <$> opt (flagOnly shortForm <|> flagOnly longForm)

inputFiles :: Args [String]
inputFiles = many argOnly

outputFile :: Args String
outputFile = flagArg "o" <|> flagArg "output"

noTco :: Args Boolean
noTco = flagOpt "no-tco"

performRuntimeTypeChecks :: Args Boolean
performRuntimeTypeChecks = flagOpt "runtime-type-checks"

noPrelude :: Args Boolean
noPrelude = flagOpt "no-prelude"

noMagicDo :: Args Boolean
noMagicDo = flagOpt "no-magic-do"

noOpts :: Args Boolean
noOpts = flagOpt "no-opts"

verboseErrors :: Args Boolean
verboseErrors = flag "v" "verbose-errors"

options :: Args Options
options = mkOptions <$> noPrelude 
                    <*> noTco 
                    <*> performRuntimeTypeChecks 
                    <*> noMagicDo 
                    <*> pure Nothing 
                    <*> noOpts 
                    <*> pure Nothing
                    <*> pure [] 
                    <*> pure [] 
                    <*> verboseErrors

term :: Args (Eff (fs :: FS, trace :: Trace, process :: Process) {})
term = runCompiler <$> outputFile <*> options <*> inputFiles

main = do
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return {}

