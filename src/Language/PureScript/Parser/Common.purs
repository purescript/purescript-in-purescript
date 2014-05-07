-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Constants, and utility functions to be used when parsing
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Common where
  
import Text.Parsing.StringParser
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Combinators

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")