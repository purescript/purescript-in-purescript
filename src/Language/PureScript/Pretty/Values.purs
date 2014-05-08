module Language.PureScript.Pretty.Values
  ( prettyPrintValue
  , prettyPrintBinder
  ) where

import Control.Apply
import Control.Arrow ((<+>))
import Control.Arrow.Kleisli (runKleisli)
import Control.Monad.State
import Control.Monad.State.Trans
import Data.Array (map)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (sequence, for)
import Data.Tuple
import Text.Pretty.PatternArrows

import Language.PureScript.Declarations
import Language.PureScript.Errors (theImpossibleHappened)
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (prettyPrintType)

literals :: Unit -> Pattern PrinterState Value String
literals _ = mkPattern' match
  where
  match :: Value -> StateT PrinterState Maybe String
  match (NumericLiteral n) = return $ show n
  match (StringLiteral s) = return $ show s
  match (BooleanLiteral true) = return "true"
  match (BooleanLiteral false) = return "false"
  match (ArrayLiteral xs) = joinWith "" <$> sequence
    [ return "[ "
    , withIndent $ prettyPrintMany prettyPrintValue' xs
    , return " ]"
    ]
  match (ObjectLiteral []) = return "{}"
  match (ObjectLiteral ps) = joinWith "" <$> sequence
    [ return "{\n"
    , withIndent $ prettyPrintMany prettyPrintObjectProperty ps
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (Constructor name) = return $ show name
  match (Case values binders) = joinWith "" <$> sequence
    [ return "case "
    , joinWith " " <$> for values prettyPrintValue'
    , return " of\n"
    , withIndent $ prettyPrintMany prettyPrintCaseAlternative binders
    , currentIndent
    ]
  match (Let ds val) = joinWith "" <$> sequence
    [ return "let\n"
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    , return "\n"
    , currentIndent
    , return "in "
    , prettyPrintValue' val
    ]
  match (Var ident) = return $ show ident
  match (Do els) = joinWith "" <$> sequence
    [ return "do "
    , withIndent $ prettyPrintMany prettyPrintDoNotationElement els
    , currentIndent
    ]
  match (TypeClassDictionary _ _ _) = return "<<dict>>"
  match (SuperClassDictionary _ _) = return "<<superclass dict>>"
  match (TypedValue _ val _) = prettyPrintValue' val
  match (PositionedValue _ val) = prettyPrintValue' val
  match _ = return ""

prettyPrintDeclaration :: Declaration -> StateT PrinterState Maybe String
prettyPrintDeclaration (TypeDeclaration ident ty) = return $ show ident ++ " :: " ++ prettyPrintType ty
prettyPrintDeclaration (ValueDeclaration ident _ [] Nothing val) = joinWith "" <$> sequence
  [ return $ show ident ++ " = "
  , prettyPrintValue' val
  ]
prettyPrintDeclaration (PositionedDeclaration _ d) = prettyPrintDeclaration d
prettyPrintDeclaration _ = theImpossibleHappened "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: CaseAlternative -> StateT PrinterState Maybe String
prettyPrintCaseAlternative (CaseAlternative { binders = binders, guard = grd, result = val }) =
  joinWith "" <$> sequence
    [ joinWith ", " <$> for binders prettyPrintBinder'
    , maybe (return "") ((<$>) ((++) "| ") <<< prettyPrintValue') grd
    , return " -> "
    , prettyPrintValue' val
    ]

prettyPrintDoNotationElement :: DoNotationElement -> StateT PrinterState Maybe String
prettyPrintDoNotationElement (DoNotationValue val) =
  prettyPrintValue' val
prettyPrintDoNotationElement (DoNotationBind binder val) =
  joinWith "" <$> sequence
    [ prettyPrintBinder' binder
    , return " <- "
    , prettyPrintValue' val
    ]
prettyPrintDoNotationElement (DoNotationLet ds) =
  joinWith "" <$> sequence
    [ return "let "
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    ]
prettyPrintDoNotationElement (PositionedDoNotationElement _ el) = prettyPrintDoNotationElement el

ifThenElse :: Pattern PrinterState Value (Tuple (Tuple Value Value) Value)
ifThenElse = mkPattern match
  where
  match (IfThenElse cond th el) = Just (Tuple (Tuple th el) cond)
  match _ = Nothing

accessor :: Pattern PrinterState Value (Tuple String Value)
accessor = mkPattern match
  where
  match (Accessor prop val) = Just (Tuple prop val)
  match _ = Nothing

objectUpdate :: Unit -> Pattern PrinterState Value (Tuple [String] Value)
objectUpdate _ = mkPattern match
  where
  match (ObjectUpdate o ps) = Just (Tuple (flip map ps $ \(Tuple key val) -> key ++ " = " ++ prettyPrintValue val) o)
  match _ = Nothing

app :: Unit -> Pattern PrinterState Value (Tuple String Value)
app _ = mkPattern match
  where
  match (App val arg) = Just (Tuple (prettyPrintValue arg) val)
  match _ = Nothing

lam :: Pattern PrinterState Value (Tuple String Value)
lam = mkPattern match
  where
  match (Abs (Left arg) val) = Just (Tuple (show arg) val)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing an expression
--
prettyPrintValue :: Value -> String
prettyPrintValue p = runPretty (flip evalStateT (PrinterState { indent: 0 }) <<< prettyPrintValue') p

prettyPrintValue' :: Value -> StateT PrinterState Maybe String
prettyPrintValue' v = runPattern matchValue v
  where
  matchValue :: Pattern PrinterState Value String
  matchValue = fix $ \p -> buildPrettyPrinter operators (literals unit <+> (parens <$> p))
  operators :: OperatorTable PrinterState Value String
  operators =
    OperatorTable [ [ Operator (wrap accessor $ \prop val -> val ++ "." ++ prop) ]
                  , [ Operator (wrap (objectUpdate unit) $ \ps val -> val ++ "{ " ++ joinWith ", " ps ++ " }") ]
                  , [ Operator (wrap (app unit) $ \arg val -> val ++ "(" ++ arg ++ ")") ]
                  , [ Operator (split lam $ \arg val -> "\\" ++ arg ++ " -> " ++ prettyPrintValue val) ]
                  , [ Operator (wrap ifThenElse $ \(Tuple th el) cond -> "if " ++ cond ++ " then " ++ prettyPrintValue th ++ " else " ++ prettyPrintValue el) ]
                  ]

prettyPrintBinderAtom :: Unit -> Pattern PrinterState Binder String
prettyPrintBinderAtom _ = mkPattern' match
  where
  match :: Binder -> StateT PrinterState Maybe String
  match NullBinder = return "_"
  match (StringBinder str) = return $ show str
  match (NumberBinder num) = return $ show num
  match (BooleanBinder true) = return "true"
  match (BooleanBinder false) = return "false"
  match (VarBinder ident) = return $ show ident
  match (ConstructorBinder ctor args) = joinWith "" <$> sequence
    [ return $ show ctor ++ " "
    , joinWith " " <$> for args match
    ]
  match (ObjectBinder bs) = joinWith "" <$> sequence
    [ return "{\n"
    , withIndent $ prettyPrintMany prettyPrintObjectPropertyBinder bs
    , currentIndent
    , return "}"
    ]
  match (ArrayBinder bs) = joinWith "" <$> sequence
    [ return "["
    , joinWith " " <$> for bs prettyPrintBinder'
    , return "]"
    ]
  match (NamedBinder ident binder) = ((++) (show ident ++ "@")) <$> prettyPrintBinder' binder
  match (PositionedBinder _ binder) = prettyPrintBinder' binder
  match _ = return ""

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder b = runPretty (flip evalStateT (PrinterState { indent: 0 }) <<< prettyPrintBinder') b

prettyPrintBinder' :: Binder -> StateT PrinterState Maybe String
prettyPrintBinder' b = runPattern matchBinder b
  where
  matchBinder :: Pattern PrinterState Binder String
  matchBinder = fix $ \p -> buildPrettyPrinter operators (prettyPrintBinderAtom unit <+> (parens <$> p))
  operators :: OperatorTable PrinterState Binder String
  operators =
    OperatorTable [ [ Operator (assocR matchConsBinder (\b1 b2 -> b1 ++ " : " ++ b2)) ] ]

matchConsBinder :: Pattern PrinterState Binder (Tuple Binder Binder)
matchConsBinder = mkPattern match'
  where
  match' (ConsBinder b1 b2) = Just (Tuple b1 b2)
  match' _ = Nothing

prettyPrintObjectPropertyBinder :: (Tuple String Binder) -> StateT PrinterState Maybe String
prettyPrintObjectPropertyBinder (Tuple key binder) = joinWith "" <$> sequence
  [ return $ prettyPrintObjectKey key ++ ": "
  , prettyPrintBinder' binder
  ]

prettyPrintObjectProperty :: (Tuple String Value) -> StateT PrinterState Maybe String
prettyPrintObjectProperty (Tuple key value) = joinWith "" <$> sequence
  [ return $ prettyPrintObjectKey key ++ ": "
  , prettyPrintValue' value
  ]
