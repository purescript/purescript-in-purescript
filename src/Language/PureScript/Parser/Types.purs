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
    parseType,
    parseTypeAtom
  ) where

import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Lazy
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

parseType :: Parser TokenStream Type
parseType = snd parseType_

parseTypeAtom :: Parser TokenStream Type
parseTypeAtom = fst parseType_

parseType_ :: Tuple (Parser TokenStream Type) (Parser TokenStream Type)
parseType_ = fix $ \(Tuple atom p) ->
  let
    -- |
    -- Parse a type as it appears in e.g. a data constructor
    --
    atom' :: Parser TokenStream Type
    atom' = choice (map try
        [ parseNumber
        , parseString
        , parseBoolean
        , parseArray
        , parseArrayOf
        , parseFunction
        , parseObject
        , parseTypeVariable
        , parseTypeConstructor
        , parseForAll
        , parens (parseRow true)
        , parens p ])

    parseNumber :: Parser TokenStream Type
    parseNumber = const tyNumber <$> uname' "Number"

    parseString :: Parser TokenStream Type
    parseString = const tyString <$> uname' "String"

    parseBoolean :: Parser TokenStream Type
    parseBoolean = const tyBoolean <$> uname' "Boolean"

    parseArray :: Parser TokenStream Type
    parseArray = squares $ return tyArray

    parseArrayOf :: Parser TokenStream Type
    parseArrayOf = squares $ TypeApp tyArray <$> p

    parseFunction :: Parser TokenStream Type
    parseFunction = parens $ try rarrow *> return tyFunction

    parseObject :: Parser TokenStream Type
    parseObject = braces $ TypeApp tyObject <$> parseRow false

    parseTypeVariable :: Parser TokenStream Type
    parseTypeVariable = do
      ident <- lname
      when (ident `elem` reservedTypeNames) $ fail ("Unexpected " ++ show ident)
      return $ TypeVar ident

    parseTypeConstructor :: Parser TokenStream Type
    parseTypeConstructor = TypeConstructor <$> parseQualified properName

    parseForAll :: Parser TokenStream Type
    parseForAll = mkForAll <$> (try (reserved "forall") *> some lname <* dot)
                           <*> parseConstrainedType

    parseConstrainedType :: Parser TokenStream Type
    parseConstrainedType = do
      constraints <- optionMaybe <<< try $ do
        constraints <- parens <<< commaSep1 $ do
          className <- parseQualified properName
          ty <- many atom
          return (Tuple className ty)
        rfatArrow
        return constraints
      ty <- p
      return $ maybe ty (flip ConstrainedType ty) constraints

    parseNameAndType :: forall t. Parser TokenStream t -> Parser TokenStream (Tuple String t)
    parseNameAndType p = Tuple <$> ((lname <|> stringLiteral) <* doubleColon) <*> p

    parseRowEnding :: Parser TokenStream Type
    parseRowEnding = option REmpty (TypeVar <$> (pipe *> lname))

    parseRow :: Boolean -> Parser TokenStream Type
    parseRow nonEmpty = (curry rowFromList <$> many' (parseNameAndType p) <*> parseRowEnding) <?> "row"
      where many' = if nonEmpty then commaSep1 else commaSep

  in Tuple atom' (buildExprParser operators atom' <?> "type")
  where
  operators = [ [ Infix (return TypeApp) AssocLeft ]
              , [ Infix (try rarrow *> return function) AssocRight ] ]
