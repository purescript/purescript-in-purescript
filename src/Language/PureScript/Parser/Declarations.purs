-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Declarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parsers for module definitions and declarations
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Declarations {-(
    parseDeclaration,
    parseModule,
    parseModules,
    parseValue,
    parseGuard,
    parseBinder,
    parseBinderNoParens,
)-} where

import Data.Tuple
import Data.Maybe

import Control.Apply

import Language.PureScript.Pos
import Language.PureScript.Declarations
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Common

import qualified Language.PureScript.Parser.Lexer as L

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Expr as P

parseDataDeclaration :: P.Parser [L.Token] Declaration
parseDataDeclaration = do
  reserved "data"
  name <- properName
  tyArgs <- P.many identifier
  ctors <- P.option [] $ do
    equals
    P.sepBy1 (Tuple <$> properName <*> P.many parseTypeAtom) pipe
  return $ DataDeclaration name tyArgs ctors
  
parseTypeDeclaration :: P.Parser [L.Token] Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (ident <* doubleColon)
                  <*> parseType
                  
parseTypeSynonymDeclaration :: P.Parser [L.Token] Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (reserved "type" *> properName)
                         <*> P.many identifier
                         <*> (equals *> parseType)
         
{-                
parseValueDeclaration :: P.Parser [L.Token] Declaration
parseValueDeclaration = do
  name <- ident
  binders <- P.many parseBinderNoParens
  guard <- P.optionMaybe parseGuard
  value <- equals *> parseValue
  whereClause <- P.optionMaybe $ do
    reserved "where"
    braces (P.semiSep1 parseLocalDeclaration)
    
  return $ ValueDeclaration name Value binders guard (maybe value (\ds -> Let ds value) whereClause)
-}
                       
parseExternDeclaration :: P.Parser [L.Token] Declaration
parseExternDeclaration = reserved "foreign" *> reserved "import" *> 
   ((do reserved "data"
        name <- properName
        doubleColon
        kind <- parseKind
        return $ ExternDataDeclaration name kind)
    <|> (do reserved "instance"
            name <- ident
            doubleColon
            deps <- P.option [] do
              deps <- parens (commaSep1 (Tuple <$> parseQualified properName <*> P.many parseTypeAtom))
              rfatArrow
              return deps
            className <- parseQualified properName
            tys <- P.many parseTypeAtom
            return $ ExternInstanceDeclaration name deps className tys)
    <|> (do ident <- ident
            js <- P.optionMaybe (JSRaw <$> stringLiteral)
            doubleColon
            ty <- parseType
            return $ ExternDeclaration (if isJust js then InlineJavascript else ForeignImport) ident js ty))
            
parseAssociativity :: P.Parser [L.Token] Associativity
parseAssociativity =
  (reserved "infixl" *> return Infixl) <|>
  (reserved "infixr" *> return Infixr) <|>
  (reserved "infix"  *> return Infix)

parseFixity :: P.Parser [L.Token] Fixity
parseFixity = Fixity <$> parseAssociativity <*> natural

parseFixityDeclaration :: P.Parser [L.Token] Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  name <- symbol
  return $ FixityDeclaration fixity name
  
parseDeclarationRef :: P.Parser [L.Token] DeclarationRef
parseDeclarationRef = PositionedDeclarationRef <$> sourcePos <*>
  (ValueRef <$> ident
   <|> do name <- properName
          dctors <- P.optionMaybe $ parens ((symbol' ".." *> pure Nothing) <|> Just <$> commaSep properName)
          return $ maybe (TypeClassRef name) (TypeRef name) dctors)  
  
parseImportDeclaration :: P.Parser [L.Token] Declaration
parseImportDeclaration = do
  reserved "import"
  qualImport <|> stdImport
  where
  
  stdImport :: P.Parser [L.Token] Declaration
  stdImport = do
    moduleName' <- moduleName
    idents <- P.optionMaybe (parens $ commaSep parseDeclarationRef)
    return $ ImportDeclaration moduleName' idents Nothing
  
  qualImport :: P.Parser [L.Token] Declaration
  qualImport = do
    reserved "qualified"
    moduleName' <- moduleName
    idents <- P.optionMaybe (parens $ commaSep parseDeclarationRef)
    reserved "as"
    asQ <- moduleName
    return $ ImportDeclaration moduleName' idents (Just asQ)
    
parseTypeClassDeclaration :: P.Parser [L.Token] Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] do
    implies <- parens (commaSep1 (Tuple <$> parseQualified properName <*> P.many parseTypeAtom))
    lfatArrow
    return implies
  className <- properName
  idents <- P.many identifier
  members <- P.option [] <<< P.try $ do
    reserved "where"
    braces (semiSep1 (positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies members
  
positioned :: P.Parser [L.Token] Declaration -> P.Parser [L.Token] Declaration
positioned d = PositionedDeclaration <$> sourcePos <*> d

-- |
-- Parse a single declaration
--
parseDeclaration :: P.Parser [L.Token] Declaration
parseDeclaration = positioned (P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   -- , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseImportDeclaration
                   , parseTypeClassDeclaration
                   -- , parseTypeInstanceDeclaration
                   ]) P.<?> "declaration"
           
-- |
-- Parse a declaration which can appear inside a let binding
--  
parseLocalDeclaration :: P.Parser [L.Token] Declaration
parseLocalDeclaration = PositionedDeclaration <$> sourcePos <*> P.choice
                   [ parseTypeDeclaration
                   -- , parseValueDeclaration
                   ] P.<?> "local declaration"

-- |
-- Parse a module header and a collection of declarations
--
parseModule :: P.Parser [L.Token] Module
parseModule = do
  reserved "module"
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  reserved "where"
  decls <- braces (semiSep parseDeclaration)
  return $ Module name decls exports