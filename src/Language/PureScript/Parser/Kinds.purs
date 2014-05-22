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

import Control.Apply

import Language.PureScript.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.Common

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators 
import Text.Parsing.Parser.Expr

parseStar :: Parser TokenStream Kind
parseStar = const Star <$> symbol' "*"

parseBang :: Parser TokenStream Kind
parseBang = const Bang <$> symbol' "!"

-- |
-- Parse a kind
--
parseKind :: Parser TokenStream Kind
parseKind = fix (\parseKind -> 
  let 
    parseKindAtom :: Parser TokenStream Kind
    parseKindAtom = choice
      [ parseStar
      , parseBang
      , parens parseKind ]
    
  in buildExprParser operators parseKindAtom)
  where
  operators = [ [ Prefix (symbol' "#" *> return Row) ]
              , [ Infix (rarrow *> return FunKind) AssocRight ] ]