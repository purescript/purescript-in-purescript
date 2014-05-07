-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Kinds
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A parser for kinds
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Kinds (
    parseKind
  ) where

import Language.PureScript.Kinds
--import Language.PureScript.Parser.State 
import Language.PureScript.Parser.Common

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators 
import Text.Parsing.StringParser.Expr
import Text.Parsing.StringParser.String

parseStar :: Parser Kind
parseStar = const Star <$> {- lexeme -} (string "*")

parseBang :: Parser Kind
parseBang = const Bang <$> {- lexeme -} (string "!")

-- |
-- Parse a kind
--
parseKind :: Parser Kind
parseKind = fix (\parseKind -> 
  let 
    parseKindAtom :: Parser Kind
    parseKindAtom = {- indented *> -}choice
                [ parseStar
                , parseBang
                , parens parseKind ]
    
  in buildExprParser operators parseKindAtom <?> "kind")
  where
  operators = [ [ Prefix ({- lexeme -} (string "#") >>= \_ -> return Row) ]
              , [ Infix ({- lexeme -} (try (string "->")) >>= \_ -> return FunKind) AssocRight ] ]