-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.JS
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for the Javascript AST
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.JS (
    prettyPrintJS
  ) where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Data.Either
import Data.Traversable (for, traverse, sequence)

import qualified Data.String as S

import Text.Pretty.PatternArrows

import Control.Arrow ((<+>))
import Control.Apply

import Control.Monad.Trans

import Control.Monad.State
import Control.Monad.State.Trans

import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Common (isIdent)
import Language.PureScript.CodeGen.JS.AST

foreign import string 
  "function string(s) {\
  \  return JSON.stringify(s);\
  \}" :: String -> String
  
literals :: Unit -> Pattern PrinterState JS String
literals _ = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ show n
  match (JSStringLiteral s) = return $ string s
  match (JSBooleanLiteral true) = return "true"
  match (JSBooleanLiteral false) = return "false"
  match (JSArrayLiteral xs) = S.joinWith "" <$> sequence
    [ return "[ "
    , S.joinWith ", " <$> for xs prettyPrintJS'
    , return " ]"
    ]
  match (JSObjectLiteral []) = return "{}"
  match (JSObjectLiteral ps) = S.joinWith "" <$> sequence
    [ return "{\n"
    , withIndent $ do
        jss <- for ps $ \(Tuple key value) -> (<$>) (\s -> (objectPropertyToString key ++ ": ") ++ s) <<< prettyPrintJS' $ value
        indentString <- currentIndent
        return $ S.joinWith ", \n" $ map (\s -> indentString ++ s) jss
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    objectPropertyToString :: String -> String
    objectPropertyToString s | isIdent s = s
    objectPropertyToString s = show s
  match (JSBlock sts) = S.joinWith "" <$> sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSVar ident) = return ident
  match (JSVariableIntroduction ident value) = S.joinWith "" <$> sequence
    [ return "var "
    , return ident
    , maybe (return "") ((<$>) (\s -> " = " ++ s) <<< prettyPrintJS') value
    ]
  match (JSAssignment target value) = S.joinWith "" <$> sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    ]
  match (JSWhile cond sts) = S.joinWith "" <$> sequence
    [ return "while ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = S.joinWith "" <$> sequence
    [ return $ "for (var " ++ ident ++ " = "
    , prettyPrintJS' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintJS' end
    , return $ "; " ++ ident ++ "++) "
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = S.joinWith "" <$> sequence
    [ return $ "for (var " ++ ident ++ " in "
    , prettyPrintJS' obj
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = S.joinWith "" <$> sequence
    [ return "if ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' thens
    , maybe (return "") ((<$>) (\s -> " else " ++ s) <<< prettyPrintJS') elses
    ]
  match (JSReturn value) = S.joinWith "" <$> sequence
    [ return "return "
    , prettyPrintJS' value
    ]
  match (JSThrow value) = S.joinWith "" <$> sequence
    [ return "throw "
    , prettyPrintJS' value
    ]
  match (JSBreak lbl) = return $ "break " ++ lbl
  match (JSContinue lbl) = return $ "continue " ++ lbl
  match (JSLabel lbl js) = S.joinWith "" <$> sequence
    [ return $ lbl ++ ": "
    , prettyPrintJS' js
    ]
  match (JSRaw js) = return js
  match _ = lift Nothing

conditional :: Pattern PrinterState JS (Tuple (Tuple JS JS) JS)
conditional = mkPattern match
  where
  match (JSConditional cond th el) = Just (Tuple (Tuple th el) cond)
  match _ = Nothing

accessor :: Unit -> Pattern PrinterState JS (Tuple String JS)
accessor _ = mkPattern match
  where
  match (JSAccessor prop val) = Just (Tuple prop val)
  match _ = Nothing

indexer :: Unit -> Pattern PrinterState JS (Tuple String JS)
indexer _ = mkPattern' match
  where
  match (JSIndexer index val) = Tuple <$> prettyPrintJS' index <*> pure val
  match _ = lift Nothing

lam :: Pattern PrinterState JS (Tuple (Tuple (Maybe String) [String]) JS)
lam = mkPattern match
  where
  match (JSFunction name args ret) = Just (Tuple (Tuple name args) ret)
  match _ = Nothing

app :: Unit -> Pattern PrinterState JS (Tuple String JS)
app _ = mkPattern' match
  where
  match (JSApp val args) = do
    jss <- traverse prettyPrintJS' args
    return (Tuple (S.joinWith ", " jss) val)
  match _ = lift Nothing

typeOf :: Pattern PrinterState JS (Tuple Unit JS)
typeOf = mkPattern match
  where
  match (JSTypeOf val) = Just (Tuple unit val)
  match _ = Nothing

unary :: UnaryOperator -> String -> Operator PrinterState JS String
unary op str = Operator (wrap match (++))
  where
  match :: Pattern PrinterState JS (Tuple String JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (Tuple str val)
    match' _ = Nothing

binary :: BinaryOperator -> String -> Operator PrinterState JS String
binary op str = Operator (assocR match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2))
  where
  match :: Pattern PrinterState JS (Tuple JS JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (Tuple v1 v2)
    match' _ = Nothing

prettyStatements :: [JS] -> StateT PrinterState Maybe String
prettyStatements sts = do
  jss <- for sts prettyPrintJS'
  indentString <- currentIndent
  return $ S.joinWith "\n" $ map (\s -> indentString ++ s ++ ";") jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
prettyPrintJS1 :: JS -> String
prettyPrintJS1 js = runPretty (flip evalStateT (PrinterState { indent: 0 }) <<< prettyPrintJS') js

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintJS :: [JS] -> String
prettyPrintJS jss = runPretty (flip evalStateT (PrinterState { indent: 0 }) <<< prettyStatements) jss

-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintJS' :: JS -> StateT PrinterState Maybe String
prettyPrintJS' js = runPattern matchValue js
  where
  matchValue :: Pattern PrinterState JS String
  matchValue = fix $ \p -> buildPrettyPrinter operators (literals unit <+> (<$>) parens p)
  operators :: OperatorTable PrinterState JS String
  operators =
    OperatorTable [ [ Operator (wrap (accessor unit) $ \prop val -> val ++ "." ++ prop) ]
                  , [ Operator (wrap (indexer unit) $ \index val -> val ++ "[" ++ index ++ "]") ]
                  , [ Operator (wrap (app unit) $ \args val -> val ++ "(" ++ args ++ ")") ]
                  , [ Operator (wrap lam $ \(Tuple name args) ret -> "function "
                        ++ fromMaybe "" name
                        ++ "(" ++ S.joinWith ", " args ++ ") "
                        ++ ret) ]
                  , [ binary    LessThan             "<" ]
                  , [ binary    LessThanOrEqualTo    "<=" ]
                  , [ binary    GreaterThan          ">" ]
                  , [ binary    GreaterThanOrEqualTo ">=" ]
                  , [ Operator (wrap typeOf $ \_ s -> "typeof " ++ s) ]
                  , [ unary     Not                  "!" ]
                  , [ unary     BitwiseNot           "~" ]
                  , [ unary     Negate               "-" ]
                  , [ unary     Positive             "+" ]
                  , [ binary    Multiply             "*" ]
                  , [ binary    Divide               "/" ]
                  , [ binary    Modulus              "%" ]
                  , [ binary    Add                  "+" ]
                  , [ binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<" ]
                  , [ binary    ShiftRight           ">>" ]
                  , [ binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    EqualTo              "===" ]
                  , [ binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Operator (wrap conditional $ \(Tuple th el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el) ]
                    ]