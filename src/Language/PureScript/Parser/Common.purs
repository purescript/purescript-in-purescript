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
import Data.Array (null)
import Data.Foldable (elem)

import Control.Apply

import Control.Monad.State.Class
import Control.Monad.Error.Class
  
import Text.Parsing.Parser
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Combinators

import Language.PureScript.Pos
import Language.PureScript.Parser.Lexer
import Language.PureScript.Names
import Language.PureScript.Keywords

runTokenParser :: forall a. Parser [Token] a -> [Token] -> Either String a
runTokenParser p ts = case runParser ts p of
  Left (ParseError o) -> Left o.message
  Right a -> Right a

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

parens :: forall a. Parser [Token] a -> Parser [Token] a
parens = between lparen rparen

lbrace :: Parser [Token] {}
lbrace = match LBrace

rbrace :: Parser [Token] {}
rbrace = match RBrace

braces :: forall a. Parser [Token] a -> Parser [Token] a
braces = between lbrace rbrace

langle :: Parser [Token] {}
langle = match LAngle

rangle :: Parser [Token] {}
rangle = match RAngle

angles :: forall a. Parser [Token] a -> Parser [Token] a
angles = between langle rangle

lsquare :: Parser [Token] {}
lsquare = match LSquare

rsquare :: Parser [Token] {}
rsquare = match RSquare

squares :: forall a. Parser [Token] a -> Parser [Token] a
squares = between lsquare rsquare

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

semi :: Parser [Token] {}
semi = match Semi

semiSep :: forall a. Parser [Token] a -> Parser [Token] [a]
semiSep p = sepBy p semi

semiSep1 :: forall a. Parser [Token] a -> Parser [Token] [a]
semiSep1 p = sepBy1 p semi

commaSep :: forall a. Parser [Token] a -> Parser [Token] [a]
commaSep p = sepBy p comma

commaSep1 :: forall a. Parser [Token] a -> Parser [Token] [a]
commaSep1 p = sepBy1 p comma

lname :: Parser [Token] String
lname = token' "identifier" go
  where
  go (LName s) = Just s
  go _ = Nothing
  
reserved :: String -> Parser [Token] {}
reserved s = token' (show s) go
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

identifier :: Parser [Token] String
identifier = do
  s <- lname
  if (s `elem` reservedPsNames) 
    then fail "Unexpected keyword" 
    else return s

-- |
-- Parse an identifier
--
ident :: Parser [Token] Ident
ident = (Ident <$> identifier) <|> (Op <$> parens symbol)

-- |
-- Parse an identifier in backticks or an operator
--
parseIdentInfix :: Parser [Token] (Qualified Ident)
parseIdentInfix = between tick tick (parseQualified (Ident <$> lname)) <|> (parseQualified (Op <$> symbol))

-- |
-- Parse a proper name
--
properName :: Parser [Token] ProperName
properName = ProperName <$> uname

-- |
-- Parse a module name
--
moduleName :: Parser [Token] ModuleName
moduleName = ModuleName <$> (sepBy properName dot)

notFollowedBy :: forall s a. String -> Parser s a -> Parser s {}
notFollowedBy name p = try $ (do
  c <- p
  fail ("Unexpected " ++ name)) <|> return {}

-- |
-- Parse a qualified name, i.e. M.name or just name
--
parseQualified :: forall a. Parser [Token] a -> Parser [Token] (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- try (properName <* delimiter)
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> try parser)
           
  delimiter :: Parser [Token] {}
  delimiter = dot <* notFollowedBy "dot" dot
  
  updatePath :: [ProperName] -> ProperName -> [ProperName]
  updatePath path name = path ++ [name]
  
  qual :: [ProperName] -> Maybe ModuleName
  qual path = if null path then Nothing else Just $ ModuleName path
  
-- |
-- TODO: A parser which returns the current source position
--
sourcePos :: Parser [Token] SourcePos
sourcePos = return $ mkSourcePos "" 0 0
