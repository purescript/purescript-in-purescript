module Main where

import Debug.Trace

import Data.Maybe
import Data.Tuple
import Data.Either

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State.Class

import Language.PureScript.Declarations
import Language.PureScript.Supply
import Language.PureScript.Sugar
import Language.PureScript.TypeChecker
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Environment
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS
import Language.PureScript.Supply
import Language.PureScript.Names
import Language.PureScript.Pretty.JS

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
          trace "Desugaring module"
          case evalSupplyT 0 (desugar [mod]) of
            Left err -> print err
            Right [mod'@(Module mn ds exps)] -> do
              trace $ "Desugared: " ++ show mod'
              trace "Type checking and elaborating terms"
              case runCheck defaultOptions (typeCheckModule mn ds) of
                Left err -> print err
                Right (Tuple ds' env) -> do
                  trace $ "Elaborated: "++ show ds'
                  trace "Generating code"
                  case runIdentity (evalSupplyT 0 (moduleToJs CommonJS defaultOptions (Module mn ds' exps) env)) of
                    jss -> do
                      trace $ "Generated JS: " 
                      trace $ prettyPrintJS jss

typeCheckModule :: ModuleName -> [Declaration] -> Check [Declaration]
typeCheckModule mn ds = do
  modify (\(CheckState st) -> CheckState (st { currentModule = Just mn }))
  typeCheckAll Nothing mn ds
                    