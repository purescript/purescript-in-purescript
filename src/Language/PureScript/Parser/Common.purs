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
import Data.Foldable (elem, foldl)

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
  
eof :: Parser TokenStream {}
eof = do
  ts <- get
  case unconsStream ts of
    Nothing -> return {}
    Just cons -> fail $ "Expected EOF at line " ++ show cons.head.line ++ ", column " ++ show cons.head.column 
    
token :: forall a. String -> (Token -> String) -> (Token -> Maybe a) -> Parser TokenStream a
token exp sh p = do
  ts <- get
  case unconsStream ts of
    Just cons -> 
      case p cons.head.token of
        Just a -> do
          put (Consumed true)
          put cons.tail
          return a
        Nothing -> fail $ "Expected " ++ exp ++ ", found " ++ sh cons.head.token ++ " at line " ++ show cons.head.line ++ ", column " ++ show cons.head.column 
    _ -> fail $ "Expected " ++ exp ++ ", found EOF"
    
token' :: forall a. String -> (Token -> Maybe a) -> Parser TokenStream a
token' exp p = token exp show p

match :: forall a. Token -> Parser TokenStream {}
match tok = token' (show tok) (\tok' -> if tok == tok' then Just {} else Nothing)
    
lparen :: Parser TokenStream {}
lparen = match LParen 

rparen :: Parser TokenStream {}
rparen = match RParen

parens :: forall a. Parser TokenStream a -> Parser TokenStream a
parens = between lparen rparen

lbrace :: Parser TokenStream {}
lbrace = match LBrace

rbrace :: Parser TokenStream {}
rbrace = match RBrace

braces :: forall a. Parser TokenStream a -> Parser TokenStream a
braces = between lbrace rbrace

langle :: Parser TokenStream {}
langle = match LAngle

rangle :: Parser TokenStream {}
rangle = match RAngle

angles :: forall a. Parser TokenStream a -> Parser TokenStream a
angles = between langle rangle

lsquare :: Parser TokenStream {}
lsquare = match LSquare

rsquare :: Parser TokenStream {}
rsquare = match RSquare

squares :: forall a. Parser TokenStream a -> Parser TokenStream a
squares = between lsquare rsquare

larrow :: Parser TokenStream {}
larrow = match LArrow

rarrow :: Parser TokenStream {}
rarrow = match RArrow

lfatArrow :: Parser TokenStream {}
lfatArrow = match LFatArrow

rfatArrow :: Parser TokenStream {}
rfatArrow = match RFatArrow

colon :: Parser TokenStream {}
colon = match Colon

doubleColon :: Parser TokenStream {}
doubleColon = match DoubleColon

equals :: Parser TokenStream {}
equals = match Equals

pipe :: Parser TokenStream {}
pipe = match Pipe

tick :: Parser TokenStream {}
tick = match Tick

dot :: Parser TokenStream {}
dot = match Dot

comma :: Parser TokenStream {}
comma = match Comma

semi :: Parser TokenStream {}
semi = match Semi

at :: Parser TokenStream {}
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
  
reserved :: String -> Parser TokenStream {}
reserved s = token' (show s) go
  where
  go (LName s') | s == s' = Just {}
  go _ = Nothing

uname :: Parser TokenStream String
uname = token' "proper name" go
  where
  go (UName s) = Just s
  go _ = Nothing
  
uname' :: String -> Parser TokenStream {}
uname' s = token' (show s) go
  where
  go (UName s') | s == s' = Just {}
  go _ = Nothing
  
symbol :: Parser TokenStream String
symbol = token' "symbol" go
  where
  go (Symbol s) = Just s
  go _ = Nothing
  
symbol' :: String -> Parser TokenStream {}
symbol' s = token' (show s) go
  where
  go (Symbol s') | s == s' = Just {}
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

integer :: Parser TokenStream Number
integer = token' "integer" go
  where
  go (Integer n) = Just n
  go _ = Nothing

float :: Parser TokenStream Number
float = token' "floating point" go
  where
  go (Float n) = Just n
  go _ = Nothing

hex :: Parser TokenStream Number
hex = token' "hexadecimal" go
  where
  go (Hex n) = Just n
  go _ = Nothing

lineComment :: Parser TokenStream String
lineComment = token' "lineComment" go
  where
  go (LineComment s) = Just s
  go _ = Nothing

blockComment :: Parser TokenStream String
blockComment = token' "block comment" go
  where
  go (BlockComment s) = Just s
  go _ = Nothing

identifier :: Parser TokenStream String
identifier = do
  s <- lname
  if (s `elem` reservedPsNames) 
    then fail "Unexpected keyword" 
    else return s

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

notFollowedBy :: forall s a. String -> Parser s a -> Parser s {}
notFollowedBy name p = try $ (do
  c <- p
  fail ("Unexpected " ++ name)) <|> return {}

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
           
  delimiter :: Parser TokenStream {}
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