-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Operators
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Operators (
    rebracket,
    removeSignedLiterals
  ) where

import Data.Either
import Data.Maybe
import Data.Function (on)
import Data.Array
import Data.Tuple
import Data.Tuple3
import Data.Traversable (traverse)

import Control.Apply
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Error
import Control.Monad.Error.Class

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Expr as P

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Errors
import Language.PureScript.Pos

import qualified Language.PureScript.Constants as C

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket :: [Module] -> Either ErrorStack [Module]
rebracket ms = do
  let fixities = concatMap collectFixities ms
  ensureNoDuplicates $ map (\(Tuple3 i pos _) -> (Tuple i pos)) fixities
  let opTable = customOperatorTable $ map (\(Tuple3 i _ f) -> (Tuple i f)) fixities
  traverse (rebracketModule opTable) ms

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module mn ds exts) = Module mn (map f ds) exts
  where
  f = (everywhereOnValues id go id).decls
  go (UnaryMinus (NumericLiteral n)) = NumericLiteral (negate n)
  go (UnaryMinus val) = App (Var (Qualified (Just (ModuleName [ProperName C.prelude])) (Ident C.negate))) val
  go other = other

rebracketModule :: [[Tuple3 (Qualified Ident) (Value -> Value -> Value) Associativity]] -> Module -> Either ErrorStack Module
rebracketModule opTable (Module mn ds exts) =
  case everywhereOnValuesTopDownM return (matchOperators opTable) return of
    { decls = f } -> Module mn <$> (map removeParens <$> traverse f ds) <*> pure exts

removeParens :: Declaration -> Declaration
removeParens = (everywhereOnValues id go id).decls
  where
  go (Parens val) = val
  go val = val

collectFixities :: Module -> [Tuple3 (Qualified Ident) SourcePos Fixity]
collectFixities (Module moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [Tuple3 (Qualified Ident) SourcePos Fixity]
  collect (PositionedDeclaration pos (FixityDeclaration fixity name)) = [Tuple3 (Qualified (Just moduleName) (Op name)) pos fixity]
  collect (FixityDeclaration _ _) = theImpossibleHappened "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates :: [Tuple (Qualified Ident) SourcePos] -> Either ErrorStack Unit
ensureNoDuplicates m = go $ sortBy (compare `on` fst) m
  where
  go [] = return unit
  go [_] = return unit
  go ((Tuple (x@(Qualified (Just mn) name)) _) : (Tuple y pos) : _) | x == y =
    rethrow (\e -> strMsg ("Error in module " ++ show mn) <> (e :: ErrorStack)) $
      rethrowWithPosition pos $
        throwError $ mkErrorStack ("Redefined fixity for " ++ show name) Nothing
  go (_ : rest) = go rest

customOperatorTable :: [Tuple (Qualified Ident) Fixity] -> [[Tuple3 (Qualified Ident) (Value -> Value -> Value) Associativity]]
customOperatorTable fixities =
  let
    applyUserOp ident t1 = App (App (Var ident) t1)
    userOps = map (\(Tuple name (Fixity a p)) -> (Tuple3 name (applyUserOp name) (Tuple p a))) fixities
    sorted = sortBy (flip compare `on` (\(Tuple3 _ _ (Tuple p _)) -> p)) userOps
    groups = groupBy ((==) `on` (\(Tuple3 _ _ (Tuple p _)) -> p)) sorted
  in
    map (map (\(Tuple3 name f (Tuple _ a)) -> (Tuple3 name f a))) groups

type Link = Either Value (Qualified Ident)

type Chain = [Link]

matchOperators :: [[Tuple3 (Qualified Ident) (Value -> Value -> Value) Associativity]] -> Value -> Either ErrorStack Value
matchOperators ops = parseChains
  where
  parseChains :: Value -> Either ErrorStack Value
  parseChains b@(BinaryNoParens _ _ _) = bracketChain (extendChain b)
  parseChains other = return other

  extendChain :: Value -> Chain
  extendChain (BinaryNoParens name l r) = Left l : Right name : extendChain r
  extendChain other = [Left other]

  bracketChain :: Chain -> Either ErrorStack Value
  bracketChain c = either (\(P.ParseError o) -> Left (o.message `mkErrorStack` Nothing)) Right
                          (P.runParser c ((P.buildExprParser opTable parseValue <* eof) P.<?> "operator expression"))

  opTable = [P.Infix (P.try (mkParser <$> parseTicks)) P.AssocLeft]
            : map (map (\(Tuple3 name f a) -> P.Infix (P.try (matchOp name) *> return f) (toAssoc a))) ops
            ++ [[ P.Infix (P.try (mkParser <$> parseOp)) P.AssocLeft ]]

  mkParser :: Qualified Ident -> Value -> Value -> Value
  mkParser ident t1 t2 = App (App (Var ident) t1) t2

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

eof :: forall t. P.Parser Chain Unit
eof = do
  ts <- get
  case ts :: Chain of
    [] -> return unit
    _ -> P.fail "Expected EOF"

token :: forall a. String -> (Link -> Maybe a) -> P.Parser Chain a
token exp p = do
  ts <- get
  case ts of
    (t : rest) ->
      case p t of
        Just a -> do
          P.consume
          put rest
          return a
        Nothing -> P.fail $ "Expected " ++ exp ++ ", found " ++ showLink t
    _ -> P.fail $ "Expected " ++ exp ++ ", found EOF"

showLink :: Link -> String
showLink (Left _) = "expression"
showLink (Right _) = "operator"

parseValue :: P.Parser Chain Value
parseValue = token "expression" match
  where
  match (Left value) = Just value
  match _ = Nothing

parseOp :: P.Parser Chain (Qualified Ident)
parseOp = token "operator" match
  where
  match (Right (q@(Qualified _ (Op _)))) = Just q
  match _ = Nothing

parseTicks :: P.Parser Chain (Qualified Ident)
parseTicks = token "infix function" match
  where
  match (Right (q@(Qualified _ (Ident _)))) = Just q
  match _ = Nothing

matchOp :: Qualified Ident -> P.Parser Chain Unit
matchOp op = do
  ident <- parseOp
  if (ident == op)
    then return unit
    else P.fail "Expected operator"
