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
  
import Prelude.Unsafe (unsafeIndex)
  
import Data.Maybe
import Data.Either
import Data.Array (map, null, length)
import Data.Foldable (notElem, foldl)

import Control.Apply

import Control.Monad.State.Class
import Control.Monad.Error.Class
  
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

import Language.PureScript.Pos
import Language.PureScript.Parser.Lexer
import Language.PureScript.Names
import Language.PureScript.Keywords

data TokenStream = TokenStream { tokens :: [PositionedToken], position :: Number }

toTokenStream :: [PositionedToken] -> TokenStream
toTokenStream ts = TokenStream { tokens: ts, position: 0 }

unconsStream :: TokenStream -> Maybe { head :: PositionedToken, tail :: TokenStream }
unconsStream (TokenStream o) | o.position < length o.tokens = 
  Just { head: o.tokens `unsafeIndex` o.position
       , tail: TokenStream { tokens: o.tokens
                           , position: o.position + 1 
                           } 
       }
unconsStream _ = Nothing

runTokenParser :: forall a. Parser TokenStream a -> [PositionedToken] -> Either String a
runTokenParser p ts = case runParser (toTokenStream ts) p of
  Left (ParseError o) -> Left o.message
  Right a -> Right a
  
eof :: Parser TokenStream Unit
eof = do
  ts <- get
  case unconsStream ts of
    Nothing -> return unit
    Just cons -> fail $ "Expected EOF at line " ++ show cons.head.line ++ ", column " ++ show cons.head.column ++ ", found " ++ show cons.head.token
    
token :: forall a. String -> (Token -> String) -> (Token -> Maybe a) -> Parser TokenStream a
token exp sh p = do
  ts <- get
  case unconsStream ts of
    Just cons -> 
      case p cons.head.token of
        Just a -> do
          consume
          put cons.tail
          return a
        Nothing -> fail $ "Expected " ++ exp ++ ", found " ++ sh cons.head.token ++ " at line " ++ show cons.head.line ++ ", column " ++ show cons.head.column 
    _ -> fail $ "Expected " ++ exp ++ ", found EOF"
    
token' :: forall a. String -> (Token -> Maybe a) -> Parser TokenStream a
token' exp p = token exp show p

match :: forall a. Token -> Parser TokenStream Unit
match tok = token' (show tok) (\tok' -> if tok == tok' then Just unit else Nothing)
    
lparen :: Parser TokenStream Unit
lparen = match LParen 

rparen :: Parser TokenStream Unit
rparen = match RParen

parens :: forall a. Parser TokenStream a -> Parser TokenStream a
parens = between lparen rparen

lbrace :: Parser TokenStream Unit
lbrace = match LBrace

rbrace :: Parser TokenStream Unit
rbrace = match RBrace

braces :: forall a. Parser TokenStream a -> Parser TokenStream a
braces = between lbrace rbrace

langle :: Parser TokenStream Unit
langle = match LAngle

rangle :: Parser TokenStream Unit
rangle = match RAngle

angles :: forall a. Parser TokenStream a -> Parser TokenStream a
angles = between langle rangle

lsquare :: Parser TokenStream Unit
lsquare = match LSquare

rsquare :: Parser TokenStream Unit
rsquare = match RSquare

squares :: forall a. Parser TokenStream a -> Parser TokenStream a
squares = between lsquare rsquare

larrow :: Parser TokenStream Unit
larrow = match LArrow

rarrow :: Parser TokenStream Unit
rarrow = match RArrow

lfatArrow :: Parser TokenStream Unit
lfatArrow = match LFatArrow

rfatArrow :: Parser TokenStream Unit
rfatArrow = match RFatArrow

colon :: Parser TokenStream Unit
colon = match Colon

doubleColon :: Parser TokenStream Unit
doubleColon = match DoubleColon

equals :: Parser TokenStream Unit
equals = match Equals

pipe :: Parser TokenStream Unit
pipe = match Pipe

tick :: Parser TokenStream Unit
tick = match Tick

dot :: Parser TokenStream Unit
dot = match Dot

comma :: Parser TokenStream Unit
comma = match Comma

semi :: Parser TokenStream Unit
semi = match Semi

at :: Parser TokenStream Unit
at = match At

semiSep :: forall a. Parser TokenStream a -> Parser TokenStream [a]
semiSep p = sepBy p semi

semiSep1 :: forall a. Parser TokenStream a -> Parser TokenStream [a]
semiSep1 p = sepBy1 p semi

commaSep :: forall a. Parser TokenStream a -> Parser TokenStream [a]
commaSep p = sepBy p comma

commaSep1 :: forall a. Parser TokenStream a -> Parser TokenStream [a]
commaSep1 p = sepBy1 p comma

lname :: Parser TokenStream String
lname = token' "identifier" go
  where
  go (LName s) = Just s
  go _ = Nothing
  
reserved :: String -> Parser TokenStream Unit
reserved s = token' (show s) go
  where
  go (LName s') | s == s' = Just unit
  go _ = Nothing

uname :: Parser TokenStream String
uname = token' "proper name" go
  where
  go (UName s) = Just s
  go _ = Nothing
  
uname' :: String -> Parser TokenStream Unit
uname' s = token' (show s) go
  where
  go (UName s') | s == s' = Just unit
  go _ = Nothing
  
symbol :: Parser TokenStream String
symbol = token' "symbol" go
  where
  go (Symbol s) = Just s
  go LAngle     = Just "<"
  go RAngle     = Just ">"
  go LFatArrow  = Just "<="
  go RFatArrow  = Just "=>"
  go Colon      = Just ":"
  go Pipe       = Just "|"
  go Dot        = Just "."
  go Comma      = Just ","
  go At         = Just "@"
  go _ = Nothing
  
symbol' :: String -> Parser TokenStream Unit
symbol' s = token' (show s) go
  where
  go (Symbol s') | s == s'   = Just unit
  go LAngle      | s == "<"  = Just unit
  go RAngle      | s == ">"  = Just unit
  go LFatArrow   | s == "<=" = Just unit
  go RFatArrow   | s == "=>" = Just unit
  go Colon       | s == ":"  = Just unit
  go Pipe        | s == "|"  = Just unit
  go Dot         | s == "."  = Just unit
  go Comma       | s == ","  = Just unit
  go At          | s == "@"  = Just unit
  go _ = Nothing

stringLiteral :: Parser TokenStream String
stringLiteral = token' "string literal" go
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

natural :: Parser TokenStream Number
natural = token' "natural number" go
  where
  go (Natural n) = Just n
  go _ = Nothing

number :: Parser TokenStream Number
number = token' "number" go
  where
  go (ANumber n) = Just n
  go _ = Nothing

identifier :: Parser TokenStream String
identifier = token' "identifier" go
  where
  go (LName s) | s `notElem` reservedPsNames = Just s
  go _ = Nothing

-- |
-- Parse an identifier
--
ident :: Parser TokenStream Ident
ident = (Ident <$> identifier) <|> (Op <$> parens symbol)

-- |
-- Parse an identifier in backticks or an operator
--
parseIdentInfix :: Parser TokenStream (Qualified Ident)
parseIdentInfix = between tick tick (parseQualified (Ident <$> lname)) <|> (parseQualified (Op <$> symbol))

-- |
-- Parse a proper name
--
properName :: Parser TokenStream ProperName
properName = ProperName <$> uname

-- |
-- Parse a module name
--
moduleName :: Parser TokenStream ModuleName
moduleName = ModuleName <$> (sepBy properName dot)

notFollowedBy :: forall s a. String -> Parser s a -> Parser s Unit
notFollowedBy name p = try (do
  c <- p
  fail ("Unexpected " ++ name)) <|> return unit

-- |
-- Run the first parser, then match the second if possible, applying the specified function on a successful match
--
augment :: forall s a b. Parser s a -> Parser s b -> (a -> b -> a) -> Parser s a
augment p q f = flip (maybe id $ flip f) <$> p <*> optionMaybe q

-- |
-- Run the first parser, then match the second zero or more times, applying the specified function for each match
--
fold :: forall s a b. Parser s a -> Parser s b -> (a -> b -> a) -> Parser s a
fold first more combine = do
  a <- first
  bs <- many more
  return $ foldl combine a bs

-- |
-- Build a parser from a smaller parser and a list of parsers for postfix operators
--
buildPostfixParser :: forall s a. [a -> Parser s a] -> Parser s a -> Parser s a
buildPostfixParser fs first = do
  a <- first
  go a
  where
  go a = do
    maybeA <- optionMaybe $ choice (map (\f -> f a) fs)
    case maybeA of
      Nothing -> return a
      Just a' -> go a'

-- |
-- Parse a qualified name, i.e. M.name or just name
--
parseQualified :: forall a. Parser TokenStream a -> Parser TokenStream (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- try (properName <* delimiter)
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> try parser)
           
  delimiter :: Parser TokenStream Unit
  delimiter = dot <* notFollowedBy "dot" dot
  
  updatePath :: [ProperName] -> ProperName -> [ProperName]
  updatePath path name = path ++ [name]
  
  qual :: [ProperName] -> Maybe ModuleName
  qual path = if null path then Nothing else Just $ ModuleName path
  
-- |
-- A parser which returns the current source position
--
sourcePos :: Parser TokenStream SourcePos
sourcePos = do
  ts <- get
  case unconsStream ts of
    Nothing -> return $ mkSourcePos "" 0 0
    Just cons -> return $ mkSourcePos "" cons.head.line cons.head.column
  
-- |
-- A parser which returns the comments for the next lexeme
--
comments :: Parser TokenStream [String]
comments = do
  ts <- get
  case unconsStream ts of
    Nothing -> return []
    Just cons -> return cons.head.comments