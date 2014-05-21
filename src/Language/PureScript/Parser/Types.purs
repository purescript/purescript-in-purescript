-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
-----------------------------------------------------------------------------
-- |
-- Parsers for types
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Types (
    parseType
  ) where

import Control.Apply
import Control.Monad (when, unless)

import Data.Maybe
import Data.Tuple
import Data.Array (map)
import Data.Foldable (elem)

import Language.PureScript.Types
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.Common
import Language.PureScript.Environment
import Language.PureScript.Keywords

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr

parseType :: Parser [Token] Type
parseType = fix $ \p -> 
  let
    -- |
    -- Parse a type as it appears in e.g. a data constructor
    --
    parseTypeAtom :: {} -> Parser [Token] Type
    parseTypeAtom _ = choice (map try
        [ parseNumber
        , parseString
        , parseBoolean
        , parseArray
        , parseArrayOf
        , parseFunction
        , parseObject
        , parseTypeVariable
        , parseTypeConstructor
        , parseForAll {}
        , parens (parseRow true)
        , parens p ])
        
    parseNumber :: Parser [Token] Type
    parseNumber = const tyNumber <$> uname' "Number"

    parseString :: Parser [Token] Type
    parseString = const tyString <$> uname' "String"

    parseBoolean :: Parser [Token] Type
    parseBoolean = const tyBoolean <$> uname' "Boolean"

    parseArray :: Parser [Token] Type
    parseArray = squares $ return tyArray

    parseArrayOf :: Parser [Token] Type
    parseArrayOf = squares $ TypeApp tyArray <$> p

    parseFunction :: Parser [Token] Type
    parseFunction = parens $ try rarrow *> return tyFunction

    parseObject :: Parser [Token] Type
    parseObject = braces $ TypeApp tyObject <$> parseRow false

    parseTypeVariable :: Parser [Token] Type
    parseTypeVariable = do
      ident <- lname
      when (ident `elem` reservedTypeNames) $ fail ("Unexpected " ++ show ident)
      return $ TypeVar ident

    parseTypeConstructor :: Parser [Token] Type
    parseTypeConstructor = TypeConstructor <$> parseQualified properName

    parseForAll :: {} -> Parser [Token] Type
    parseForAll _ = mkForAll <$> (try (lname' "forall") *> many1 lname <* dot)
                             <*> parseConstrainedType {}

    parseConstrainedType :: {} -> Parser [Token] Type
    parseConstrainedType _ = do
      constraints <- optionMaybe <<< try $ do
        constraints <- parens <<< commaSep1 $ do
          className <- parseQualified properName
          ty <- many (parseTypeAtom {})
          return (Tuple className ty)
        rfatArrow
        return constraints
      ty <- p
      return $ maybe ty (flip ConstrainedType ty) constraints
        
    parseNameAndType :: forall t. Parser [Token] t -> Parser [Token] (Tuple String t)
    parseNameAndType p = Tuple <$> ((lname <|> stringLiteral) <* doubleColon) <*> p

    parseRowEnding :: Parser [Token] Type
    parseRowEnding = option REmpty (TypeVar <$> (pipe *> lname))

    parseRow :: Boolean -> Parser [Token] Type
    parseRow nonEmpty = (curry rowFromList <$> many' (parseNameAndType p) <*> parseRowEnding) <?> "row"
      where many' = if nonEmpty then commaSep1 else commaSep    
        
  in buildExprParser operators (parseTypeAtom {}) <?> "type"
  where
  operators = [ [ Infix (return TypeApp) AssocLeft ]
              , [ Infix (try rarrow *> return function) AssocRight ] ]