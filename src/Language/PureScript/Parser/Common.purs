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
  
import Data.Maybe
import Data.Either

import Control.Monad.State.Class
import Control.Monad.Error.Class
  
import Text.Parsing.Parser
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Combinators

import Language.PureScript.Parser.Lexer

runTokenParser :: forall a. Parser [Token] a -> [Token] -> Either String a
runTokenParser p ts = case runParser ts p of
  Left (ParseError o) -> Left o.message
  Right a -> Right a

parens :: forall a. Parser [Token] a -> Parser [Token] a
parens = between lparen rparen

token :: forall tok a. String -> (tok -> String) -> (tok -> Maybe a) -> Parser [tok] a
token exp sh p = do
  ts <- get
  case ts of
    (t : ts') -> case p t of
      Just a -> do
        put (Consumed true)
        put ts'
        return a
      Nothing -> fail $ "Expected " ++ exp ++ ", found " ++ sh t
    _ -> fail $ "Expected " ++ exp ++ ", found EOF"
    
token' :: forall a. String -> (Token -> Maybe a) -> Parser [Token] a
token' exp p = token exp show p

match :: forall a. Token -> Parser [Token] {}
match tok = token' (show tok) (\tok' -> if tok == tok' then Just {} else Nothing)
    
lparen :: Parser [Token] {}
lparen = match LParen 

rparen :: Parser [Token] {}
rparen = match RParen

lbrace :: Parser [Token] {}
lbrace = match LBrace

rbrace :: Parser [Token] {}
rbrace = match LBrace

langle :: Parser [Token] {}
langle = match LAngle

rangle :: Parser [Token] {}
rangle = match RAngle

lsquare :: Parser [Token] {}
lsquare = match LSquare

rsquare :: Parser [Token] {}
rsquare = match RSquare
  
indent :: Parser [Token] {}
indent = match Indent

dedent :: Parser [Token] {}
dedent = match Dedent

larrow :: Parser [Token] {}
larrow = match LArrow

rarrow :: Parser [Token] {}
rarrow = match RArrow

lfatArrow :: Parser [Token] {}
lfatArrow = match LFatArrow

rfatArrow :: Parser [Token] {}
rfatArrow = match RFatArrow

colon :: Parser [Token] {}
colon = match Colon

doubleColon :: Parser [Token] {}
doubleColon = match DoubleColon

equals :: Parser [Token] {}
equals = match Equals

pipe :: Parser [Token] {}
pipe = match Pipe

tick :: Parser [Token] {}
tick = match Tick

dot :: Parser [Token] {}
dot = match Dot

comma :: Parser [Token] {}
comma = match Comma

lname :: Parser [Token] String
lname = token' "identifier" go
  where
  go (LName s) = Just s
  go _ = Nothing
  
lname' :: String -> Parser [Token] {}
lname' s = token' (show s) go
  where
  go (LName s') | s == s' = Just {}
  go _ = Nothing

uname :: Parser [Token] String
uname = token' "proper name" go
  where
  go (UName s) = Just s
  go _ = Nothing
  
uname' :: String -> Parser [Token] {}
uname' s = token' (show s) go
  where
  go (UName s') | s == s' = Just {}
  go _ = Nothing
  
symbol :: Parser [Token] String
symbol = token' "symbol" go
  where
  go (Symbol s) = Just s
  go _ = Nothing
  
symbol' :: String -> Parser [Token] {}
symbol' s = token' (show s) go
  where
  go (Symbol s') | s == s' = Just {}
  go _ = Nothing

stringLiteral :: Parser [Token] String
stringLiteral = token' "string literal" go
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

natural :: Parser [Token] Number
natural = token' "natural number" go
  where
  go (Natural n) = Just n
  go _ = Nothing

integer :: Parser [Token] Number
integer = token' "integer" go
  where
  go (Integer n) = Just n
  go _ = Nothing

float :: Parser [Token] Number
float = token' "floating point" go
  where
  go (Float n) = Just n
  go _ = Nothing

hex :: Parser [Token] Number
hex = token' "hexadecimal" go
  where
  go (Hex n) = Just n
  go _ = Nothing

lineComment :: Parser [Token] String
lineComment = token' "lineComment" go
  where
  go (LineComment s) = Just s
  go _ = Nothing

blockComment :: Parser [Token] String
blockComment = token' "block comment" go
  where
  go (BlockComment s) = Just s
  go _ = Nothing