module Main where

import Debug.Trace

import Data.Either

import Control.Apply

import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Declarations

import Language.PureScript.Supply
import Language.PureScript.Sugar

example = 
  "module Test where\n\
  \\n\
  \  foo :: Number -> Number\n\
  \  foo 0 = 1\n\
  \  foo 1 = 0\n\
  \  foo n = n"

main = do
  trace "Lexing source file"
  case lex example of
    Left err -> trace err
    Right tokens -> do
      trace $ "Tokens: " ++ show tokens 
      trace "Parsing module"
      case runTokenParser (parseModule <* eof) tokens of
        Left err -> trace err
        Right mod -> do
          trace $ "Module: " ++ show mod
          trace "Desugaring module"
          case evalSupplyT 0 (desugar [mod]) of
            Left err -> print err
            Right [mod'] -> do
              trace $ "Desugared: " ++ show mod'
              
