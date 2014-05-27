module Main where

import Debug.Trace

import Data.Maybe
import Data.Tuple
import Data.Either

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State.Class

import Language.PureScript
import Language.PureScript.Declarations
import Language.PureScript.Options

import qualified Language.PureScript.Parser.Lexer as P
import qualified Language.PureScript.Parser.Common as P
import qualified Language.PureScript.Parser.Declarations as P

example = 
  "module Test where\n\
  \\n\
  \  foo :: Number -> Number\n\
  \  foo 0 = 1\n\
  \  foo 1 = 0\n\
  \  foo n = n"

main = do
  trace "Lexing source file"
  case P.lex example of
    Left err -> trace err
    Right tokens -> do
      trace $ "Tokens: " ++ show tokens 
      trace "Parsing module"
      case P.runTokenParser (P.parseModule <* P.eof) tokens of
        Left err -> trace err
        Right mod -> do
          trace $ "Module: " ++ show mod
          trace "Compiling module"
          case compile options [mod] of
            Left err -> print err
            Right tup3 -> do
              trace $ "Compiled: " ++ show tup3
              
options :: Options
options = Options { noPrelude: true
                  , noTco: false
                  , performRuntimeTypeChecks: false
                  , noMagicDo: false
                  , main: Nothing
                  , noOptimizations: false
                  , browserNamespace: Just "PS"
                  , modules: []
                  , codeGenModules: []
                  , verboseErrors: true
                  }