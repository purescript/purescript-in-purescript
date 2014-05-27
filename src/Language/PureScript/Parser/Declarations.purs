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

module Language.PureScript.Parser.Declarations (
    parseDeclaration,
    parseModule,
    parseModules,
    parseValue,
    parseGuard,
    parseBinder,
    parseBinderNoParens
  ) where

import Data.Array (map)
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foldable (foldr)

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

parseDataDeclaration :: P.Parser TokenStream Declaration
parseDataDeclaration = do
  reserved "data"
  name <- properName
  tyArgs <- P.many identifier
  ctors <- P.option [] $ do
    equals
    P.sepBy1 (Tuple <$> properName <*> P.many parseTypeAtom) pipe
  return $ DataDeclaration name tyArgs ctors
  
parseTypeDeclaration :: P.Parser TokenStream Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (ident <* doubleColon)
                  <*> parseType
                  
parseTypeSynonymDeclaration :: P.Parser TokenStream Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (reserved "type" *> properName)
                         <*> P.many identifier
                         <*> (equals *> parseType)
                     
parseValueDeclaration :: {} -> P.Parser TokenStream Declaration
parseValueDeclaration _ = do
  name <- ident
  binders <- P.many (parseBinderNoParens {})
  guard <- P.optionMaybe (parseGuard {})
  value <- equals *> parseValue {}
  whereClause <- P.optionMaybe $ do
    reserved "where"
    braces (semiSep1 (parseLocalDeclaration {}))
  return $ ValueDeclaration name Value binders guard (maybe value (\ds -> Let ds value) whereClause)
                       
parseExternDeclaration :: P.Parser TokenStream Declaration
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
            
parseAssociativity :: P.Parser TokenStream Associativity
parseAssociativity =
  (reserved "infixl" *> return Infixl) <|>
  (reserved "infixr" *> return Infixr) <|>
  (reserved "infix"  *> return Infix)

parseFixity :: P.Parser TokenStream Fixity
parseFixity = Fixity <$> parseAssociativity <*> natural

parseFixityDeclaration :: P.Parser TokenStream Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  name <- symbol
  return $ FixityDeclaration fixity name
  
parseDeclarationRef :: P.Parser TokenStream DeclarationRef
parseDeclarationRef = PositionedDeclarationRef <$> sourcePos <*>
  (ValueRef <$> ident
   <|> do name <- properName
          dctors <- P.optionMaybe $ parens ((symbol' ".." *> pure Nothing) <|> Just <$> commaSep properName)
          return $ maybe (TypeClassRef name) (TypeRef name) dctors)  
  
parseImportDeclaration :: P.Parser TokenStream Declaration
parseImportDeclaration = do
  reserved "import"
  qualImport <|> stdImport
  where
  
  stdImport :: P.Parser TokenStream Declaration
  stdImport = do
    moduleName' <- moduleName
    idents <- P.optionMaybe (parens $ commaSep parseDeclarationRef)
    return $ ImportDeclaration moduleName' idents Nothing
  
  qualImport :: P.Parser TokenStream Declaration
  qualImport = do
    reserved "qualified"
    moduleName' <- moduleName
    idents <- P.optionMaybe (parens $ commaSep parseDeclarationRef)
    reserved "as"
    asQ <- moduleName
    return $ ImportDeclaration moduleName' idents (Just asQ)
    
parseTypeClassDeclaration :: P.Parser TokenStream Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] do
    implies <- parens (commaSep1 (Tuple <$> parseQualified properName <*> P.many parseTypeAtom))
    lfatArrow
    return implies
  className <- properName
  idents <- P.many identifier
  members <- P.option [] $ do
    reserved "where"
    braces (semiSep1 (positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies members
  
parseTypeInstanceDeclaration :: P.Parser TokenStream Declaration
parseTypeInstanceDeclaration = do
  reserved "instance"
  name <- ident <* doubleColon
  deps <- P.optionMaybe $ parens (commaSep1 (Tuple <$> parseQualified properName <*> P.many parseTypeAtom)) <* rfatArrow
  className <- parseQualified properName
  ty <- P.many parseTypeAtom
  members <- P.option [] $ do
    reserved "where"
    braces (semiSep1 (positioned (parseValueDeclaration {})))
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty members
  
positioned :: P.Parser TokenStream Declaration -> P.Parser TokenStream Declaration
positioned d = PositionedDeclaration <$> sourcePos <*> d

-- |
-- Parse a single declaration
--
parseDeclaration :: P.Parser TokenStream Declaration
parseDeclaration = positioned (P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration {}
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseImportDeclaration
                   , parseTypeClassDeclaration
                   , parseTypeInstanceDeclaration
                   ]) P.<?> "declaration"
                   
parseLocalDeclaration :: {} -> P.Parser TokenStream Declaration
parseLocalDeclaration _ = PositionedDeclaration <$> sourcePos <*> P.choice
                   [ parseTypeDeclaration
                   , parseValueDeclaration {}
                   ] P.<?> "local declaration"

-- |
-- Parse a module header and a collection of declarations
--
parseModule :: P.Parser TokenStream Module
parseModule = do
  reserved "module"
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  reserved "where"
  decls <- braces (semiSep parseDeclaration)
  return $ Module name decls exports
  
-- |
-- Parse a collection of modules
--
parseModules :: P.Parser TokenStream [Module]
parseModules = P.many parseModule <* eof

--
-- Values
--

booleanLiteral :: P.Parser TokenStream Boolean
booleanLiteral = (reserved "true" *> return true) <|> (reserved "false" *> return false)

parseNumericLiteral :: P.Parser TokenStream Value
parseNumericLiteral = NumericLiteral <$> (natural <|> integer <|> float <|> hex)

parseStringLiteral :: P.Parser TokenStream Value
parseStringLiteral = StringLiteral <$> stringLiteral

parseBooleanLiteral :: P.Parser TokenStream Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: {} -> P.Parser TokenStream Value
parseArrayLiteral _ = ArrayLiteral <$> squares (commaSep (parseValue {}))

parseObjectLiteral :: {} -> P.Parser TokenStream Value
parseObjectLiteral _ = ObjectLiteral <$> braces (commaSep (parseIdentifierAndValue {}))

parseIdentifierAndValue :: {} -> P.Parser TokenStream (Tuple String Value)
parseIdentifierAndValue _ = Tuple <$> ((identifier <|> stringLiteral) <* colon)
                                <*> parseValue {}
                    
parseAbs :: {} -> P.Parser TokenStream Value
parseAbs _ = do
  symbol' "\\"
  args <- P.many1 (Abs <$> (Left <$> P.try ident <|> Right <$> parseBinderNoParens {}))
  rarrow
  value <- parseValue {}
  return $ toFunction args value
  where
  toFunction :: [Value -> Value] -> Value -> Value
  toFunction args value = foldr ($) value args
  
parseVar :: P.Parser TokenStream Value
parseVar = Var <$> parseQualified ident

parseConstructor :: P.Parser TokenStream Value
parseConstructor = Constructor <$> parseQualified properName 

parseCase :: {} -> P.Parser TokenStream Value
parseCase _ = Case <$> P.between (P.try (reserved "case")) (reserved "of") (return <$> parseValue {})
                   <*> braces (semiSep (parseCaseAlternative {}))
                 
parseCaseAlternative :: {} -> P.Parser TokenStream CaseAlternative
parseCaseAlternative _ = mkCaseAlternative <$> (return <$> parseBinder {})
                                           <*> P.optionMaybe (parseGuard {})
                                           <*> (rarrow *> parseValue {})
                                           P.<?> "case alternative"
                                         
parseIfThenElse :: {} -> P.Parser TokenStream Value
parseIfThenElse _ = IfThenElse <$> (P.try (reserved "if") *> parseValue {})
                               <*> (reserved "then" *> parseValue {})
                               <*> (reserved "else" *> parseValue {})
          
parseDo :: {} -> P.Parser TokenStream Value
parseDo _ = do
  reserved "do"
  Do <$> braces (semiSep1 (parseDoNotationElement {}))

parseDoNotationLet :: {} -> P.Parser TokenStream DoNotationElement
parseDoNotationLet _ = DoNotationLet <$> (reserved "let" *> braces (semiSep1 (parseLocalDeclaration {})))

parseDoNotationBind :: {} -> P.Parser TokenStream DoNotationElement
parseDoNotationBind _ = DoNotationBind <$> parseBinder {} <*> (larrow *> parseValue {})

parseDoNotationElement :: {} -> P.Parser TokenStream DoNotationElement
parseDoNotationElement _ = P.choice
  [ P.try (parseDoNotationBind {})
  , parseDoNotationLet {}
  , P.try (DoNotationValue <$> parseValue {}) ]
                             
parseLet :: {} -> P.Parser TokenStream Value
parseLet _ = do
  reserved "let"
  ds <- braces (semiSep1 (parseLocalDeclaration {}))
  reserved "in"
  result <- parseValue {}
  return $ Let ds result
  
parsePropertyUpdate :: {} -> P.Parser TokenStream (Tuple String Value)
parsePropertyUpdate _ = do
  name <- identifier <|> stringLiteral
  value <- equals *> parseValue {}
  return (Tuple name value)

parseAccessor :: Value -> P.Parser TokenStream Value
parseAccessor (Constructor _) = P.fail "Unexpected constructor"
parseAccessor obj = P.try $ Accessor <$> (dot *> (identifier <|> stringLiteral)) <*> pure obj  
                                
-- |
-- Parse a value
--
parseValue :: {} -> P.Parser TokenStream Value
parseValue _ = P.fix $ \parseValue' ->
  let
  
    parseValueAtom :: P.Parser TokenStream Value
    parseValueAtom = P.choice
      [ parseNumericLiteral
      , parseStringLiteral
      , parseBooleanLiteral
      , parseArrayLiteral {}
      , P.try (parseObjectLiteral {})
      , parseAbs {}
      , P.try parseConstructor
      , P.try parseVar
      , parseCase {}
      , parseIfThenElse {}
      , parseDo {}
      , parseLet {}
      , Parens <$> parens parseValue' ]
  
    indexersAndAccessors = buildPostfixParser postfixTable1 parseValueAtom
    
    postfixTable1 = [ parseAccessor
                    , \v -> P.try $ flip ObjectUpdate <$> (braces (commaSep1 (parsePropertyUpdate {}))) <*> pure v 
                    ]
    postfixTable2 = [ \v -> P.try (flip App <$> indexersAndAccessors) <*> pure v
                    , \v -> flip (TypedValue true) <$> (doubleColon *> parseType) <*> pure v
                    ]
      
  in PositionedValue <$> sourcePos
                               <*> (P.buildExprParser operators <<< buildPostfixParser postfixTable2 $ indexersAndAccessors) 
                               P.<?> "expression"
  where
  
  operators = [ [ P.Prefix (P.try (symbol' "-") *> return UnaryMinus)
                ]
              , [ P.Infix (P.try (parseIdentInfix P.<?> "operator") >>= \ident ->
                    return (BinaryNoParens ident)) P.AssocRight
                ]
              ]

parseGuard :: {} -> P.Parser TokenStream Guard
parseGuard _ = pipe *> parseValue {}

parseStringBinder :: P.Parser TokenStream Binder
parseStringBinder = StringBinder <$> stringLiteral

parseBooleanBinder :: P.Parser TokenStream Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parser TokenStream Binder
parseNumberBinder = NumberBinder <$> natural

parseVarBinder :: P.Parser TokenStream Binder
parseVarBinder = VarBinder <$> ident

parseNullaryConstructorBinder :: P.Parser TokenStream Binder
parseNullaryConstructorBinder = ConstructorBinder <$> parseQualified properName <*> pure []

parseConstructorBinder :: {} -> P.Parser TokenStream Binder
parseConstructorBinder _ = do 
  ctor <- parseQualified properName
  binders <- P.many (parseBinderNoParens {})
  return $ ConstructorBinder ctor binders

parseObjectBinder :: {} -> P.Parser TokenStream Binder
parseObjectBinder _ = ObjectBinder <$> braces (commaSep (parseIdentifierAndBinder {}))

parseArrayBinder :: {} -> P.Parser TokenStream Binder
parseArrayBinder _ = squares $ ArrayBinder <$> commaSep (parseBinder {})

parseNamedBinder :: {} -> P.Parser TokenStream Binder
parseNamedBinder _ = do
  name <- ident
  at
  binder <- parseBinder {}
  return $ NamedBinder name binder

parseNullBinder :: P.Parser TokenStream Binder
parseNullBinder = reserved "_" *> return NullBinder

parseIdentifierAndBinder :: {} -> P.Parser TokenStream (Tuple String Binder)
parseIdentifierAndBinder _ = do
  name <- identifier <|> stringLiteral
  equals
  binder <- parseBinder {}
  return (Tuple name binder)

-- |
-- Parse a binder
--
parseBinder :: {} -> P.Parser TokenStream Binder
parseBinder _ = P.fix $ \p -> 
  let
    parseBinderAtom :: P.Parser TokenStream Binder
    parseBinderAtom = P.choice (map P.try
                      [ parseNullBinder
                      , parseStringBinder
                      , parseBooleanBinder
                      , parseNumberBinder
                      , parseNamedBinder {}
                      , parseVarBinder
                      , parseConstructorBinder {}
                      , parseObjectBinder {}
                      , parseArrayBinder {}
                      , parens p ]) P.<?> "binder"
  in PositionedBinder <$> sourcePos <*> (P.buildExprParser operators parseBinderAtom P.<?> "expression")
  where
  operators = [ [ P.Infix (colon *> return ConsBinder) P.AssocRight ] ]

-- |
-- Parse a binder as it would appear in a top level declaration
--
parseBinderNoParens :: {} -> P.Parser TokenStream Binder
parseBinderNoParens _ = P.choice (map P.try
                  [ parseNullBinder
                  , parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseNamedBinder {}
                  , parseVarBinder
                  , parseNullaryConstructorBinder
                  , parseObjectBinder {}
                  , parseArrayBinder {}
                  , parens (parseBinder {}) ]) P.<?> "binder"