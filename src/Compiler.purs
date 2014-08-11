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
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS

import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Monad.Application
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Cont.Trans

import Node.Args
import Node.FS
import Node.Path

import Language.PureScript
import Language.PureScript.Declarations
import Language.PureScript.Options
import Language.PureScript.Prelude

import qualified Language.PureScript.Parser.Lexer as P
import qualified Language.PureScript.Parser.Common as P
import qualified Language.PureScript.Parser.Declarations as P

moduleFromText :: String -> Either String Module
moduleFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModule tokens

readInput :: forall eff. [String] -> Application [Module]
readInput input =
  for input (\inputFile -> do
    text <- readFileApplication inputFile
    case moduleFromText text of
      Left err -> throwError err
      Right m -> return m)

runCompiler :: forall eff. Options -> [String] -> Maybe String -> Maybe String -> Eff (fs :: FS, trace :: Trace, process :: Process | eff) Unit
runCompiler opts@(Options optso) input output externs = runApplication do
  modules <- readInput allInputFiles
  Tuple3 js exts _ <- eitherApplication $ compile opts modules
  case output of
    Nothing -> effApplication (trace js)
    Just path -> do
      mkdirpApplication (dirname path)
      writeFileApplication path js
  for externs $ \path -> writeFileApplication path exts
  return unit
  where
  allInputFiles :: [String]
  allInputFiles | optso.noPrelude = input
  allInputFiles = preludeFiles ++ input

flag :: String -> String -> Args Boolean
flag shortForm longForm = maybe false (const true) <$> opt (flagOnly shortForm <|> flagOnly longForm)

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

term :: Args (Eff (fs :: FS, trace :: Trace, process :: Process) Unit)
term = runCompiler <$> options <*> inputFiles <*> outputFile <*> externsFile

main = do
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return unit

