module Language.PureScript.Pretty.Kinds (prettyPrintKind) where

import Control.Arrow
import Data.Maybe
import Data.Tuple
import Text.Pretty.PatternArrows

import Language.PureScript.Kinds
import Language.PureScript.Pretty.Common

typeLiterals :: Pattern Unit Kind String
typeLiterals = mkPattern match
  where
  match Star = Just "*"
  match Bang = Just "!"
  match (KUnknown u) = Just $ "u" ++ show u
  match _ = Nothing

matchRow :: Pattern Unit Kind (Tuple Unit Kind)
matchRow = mkPattern match
  where
  match (Row k) = Just (Tuple unit k)
  match _ = Nothing

funKind :: Pattern Unit Kind (Tuple Kind Kind)
funKind = mkPattern match
  where
  match (FunKind arg ret) = Just (Tuple arg ret)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing a Kind
--
prettyPrintKind :: Kind -> String
prettyPrintKind = runPretty $ pattern matchKind unit
  where
  matchKind :: Pattern Unit Kind String
  matchKind = fix $ \p -> buildPrettyPrinter operators (typeLiterals <+> (parens <$> p))
  operators :: OperatorTable Unit Kind String
  operators =
    OperatorTable [ [ Operator (wrap matchRow $ \_ k -> "# " ++ k) ]
                  , [ Operator (assocR funKind $ \arg ret -> arg ++ " -> " ++ ret) ] ]
