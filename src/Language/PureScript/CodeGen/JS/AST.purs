module Language.PureScript.CodeGen.JS.AST where

import Prelude
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Generics

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive

instance showUnaryOperator :: Show UnaryOperator where
  show = gshow

instance eqUnaryOperator :: Eq UnaryOperator where
  (==) = geq
  (/=) x y = not (geq x y)

instance genericUnaryOperator :: Generic UnaryOperator where
  typeOf _ = TyCon { tyCon: "Language.PureScript.CodeGen.JS.AST.UnaryOperator", args: [] }
  term Negate     = TmCon { con: "Language.PureScript.Names.Negate"     , values: [] }
  term Not        = TmCon { con: "Language.PureScript.Names.Not"        , values: [] }
  term BitwiseNot = TmCon { con: "Language.PureScript.Names.BitwiseNot" , values: [] }
  term Positive   = TmCon { con: "Language.PureScript.Names.Positive"   , values: [] }
  unTerm (TmCon { con = "Language.PureScript.Names.Negate"     }) = Just Negate
  unTerm (TmCon { con = "Language.PureScript.Names.Not"        }) = Just Not
  unTerm (TmCon { con = "Language.PureScript.Names.BitwiseNot" }) = Just BitwiseNot
  unTerm (TmCon { con = "Language.PureScript.Names.Positive"   }) = Just Positive
  unTerm _ = Nothing

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- |
  -- Bitwise right shift with zero-fill
  --
  | ZeroFillShiftRight

instance showBinaryOperator :: Show BinaryOperator where
  show = gshow

instance eqBinaryOperator :: Eq BinaryOperator where
  (==) = geq
  (/=) x y = not (geq x y)
  
instance genericBinaryOperator :: Generic BinaryOperator where
  typeOf _ = TyCon { tyCon: "Language.PureScript.CodeGen.JS.AST.BinaryOperator", args: [] }
  term Add                  = TmCon { con: "Language.PureScript.Names.Add"                  , values: [] }
  term Subtract             = TmCon { con: "Language.PureScript.Names.Subtract"             , values: [] }
  term Multiply             = TmCon { con: "Language.PureScript.Names.Multiply"             , values: [] }
  term Divide               = TmCon { con: "Language.PureScript.Names.Divide"               , values: [] }
  term Modulus              = TmCon { con: "Language.PureScript.Names.Modulus"              , values: [] }
  term EqualTo              = TmCon { con: "Language.PureScript.Names.EqualTo"              , values: [] }
  term NotEqualTo           = TmCon { con: "Language.PureScript.Names.NotEqualTo"           , values: [] }
  term LessThan             = TmCon { con: "Language.PureScript.Names.LessThan"             , values: [] }
  term LessThanOrEqualTo    = TmCon { con: "Language.PureScript.Names.LessThanOrEqualTo"    , values: [] }
  term GreaterThan          = TmCon { con: "Language.PureScript.Names.GreaterThan"          , values: [] }
  term GreaterThanOrEqualTo = TmCon { con: "Language.PureScript.Names.GreaterThanOrEqualTo" , values: [] }
  term And                  = TmCon { con: "Language.PureScript.Names.And"                  , values: [] }
  term Or                   = TmCon { con: "Language.PureScript.Names.Or"                   , values: [] }
  term BitwiseAnd           = TmCon { con: "Language.PureScript.Names.BitwiseAnd"           , values: [] }
  term BitwiseOr            = TmCon { con: "Language.PureScript.Names.BitwiseOr"            , values: [] }
  term BitwiseXor           = TmCon { con: "Language.PureScript.Names.BitwiseXor"           , values: [] }
  term ShiftLeft            = TmCon { con: "Language.PureScript.Names.ShiftLeft"            , values: [] }
  term ShiftRight           = TmCon { con: "Language.PureScript.Names.ShiftRight"           , values: [] }
  term ZeroFillShiftRight   = TmCon { con: "Language.PureScript.Names.ZeroFillShiftRight"   , values: [] }
  unTerm (TmCon { con = "Language.PureScript.Names.Add"                  }) = Just Add
  unTerm (TmCon { con = "Language.PureScript.Names.Subtract"             }) = Just Subtract
  unTerm (TmCon { con = "Language.PureScript.Names.Multiply"             }) = Just Multiply
  unTerm (TmCon { con = "Language.PureScript.Names.Divide"               }) = Just Divide
  unTerm (TmCon { con = "Language.PureScript.Names.Modulus"              }) = Just Modulus
  unTerm (TmCon { con = "Language.PureScript.Names.EqualTo"              }) = Just EqualTo
  unTerm (TmCon { con = "Language.PureScript.Names.NotEqualTo"           }) = Just NotEqualTo
  unTerm (TmCon { con = "Language.PureScript.Names.LessThan"             }) = Just LessThan
  unTerm (TmCon { con = "Language.PureScript.Names.LessThanOrEqualTo"    }) = Just LessThanOrEqualTo
  unTerm (TmCon { con = "Language.PureScript.Names.GreaterThan"          }) = Just GreaterThan
  unTerm (TmCon { con = "Language.PureScript.Names.GreaterThanOrEqualTo" }) = Just GreaterThanOrEqualTo
  unTerm (TmCon { con = "Language.PureScript.Names.And"                  }) = Just And
  unTerm (TmCon { con = "Language.PureScript.Names.Or"                   }) = Just Or
  unTerm (TmCon { con = "Language.PureScript.Names.BitwiseAnd"           }) = Just BitwiseAnd
  unTerm (TmCon { con = "Language.PureScript.Names.BitwiseOr"            }) = Just BitwiseOr
  unTerm (TmCon { con = "Language.PureScript.Names.BitwiseXor"           }) = Just BitwiseXor
  unTerm (TmCon { con = "Language.PureScript.Names.ShiftLeft"            }) = Just ShiftLeft
  unTerm (TmCon { con = "Language.PureScript.Names.ShiftRight"           }) = Just ShiftRight
  unTerm (TmCon { con = "Language.PureScript.Names.ZeroFillShiftRight"   }) = Just ZeroFillShiftRight
  unTerm _ = Nothing

-- |
-- Data type for simplified Javascript expressions
--
data JS
  -- |
  -- A numeric literal
  --
  = JSNumericLiteral Number
  -- |
  -- A string literal
  --
  | JSStringLiteral String
  -- |
  -- A boolean literal
  --
  | JSBooleanLiteral Boolean
  -- |
  -- A unary operator application
  --
  | JSUnary UnaryOperator JS
  -- |
  -- A binary operator application
  --
  | JSBinary BinaryOperator JS JS
  -- |
  -- An array literal
  --
  | JSArrayLiteral [JS]
  -- |
  -- An array indexer expression
  --
  | JSIndexer JS JS
  -- |
  -- An object literal
  --
  | JSObjectLiteral [Tuple String JS]
  -- |
  -- An object property accessor expression
  --
  | JSAccessor String JS
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | JSFunction (Maybe String) [String] JS
  -- |
  -- Function application
  --
  | JSApp JS [JS]
  -- |
  -- Variable
  --
  | JSVar String
  -- |
  -- Conditional expression
  --
  | JSConditional JS JS JS
  -- |
  -- A block of expressions in braces
  --
  | JSBlock [JS]
  -- |
  -- A variable introduction and optional initialization
  --
  | JSVariableIntroduction String (Maybe JS)
  -- |
  -- A variable assignment
  --
  | JSAssignment JS JS
  -- |
  -- While loop
  --
  | JSWhile JS JS
  -- |
  -- For loop
  --
  | JSFor String JS JS JS
  -- |
  -- ForIn loop
  --
  | JSForIn String JS JS
  -- |
  -- If-then-else statement
  --
  | JSIfElse JS JS (Maybe JS)
  -- |
  -- Return statement
  --
  | JSReturn JS
  -- |
  -- Throw statement
  --
  | JSThrow JS
  -- |
  -- Type-Of operator
  --
  | JSTypeOf JS
  -- |
  -- Labelled statement
  --
  | JSLabel String JS
  -- |
  -- Break statement
  --
  | JSBreak String
  -- |
  -- Continue statement
  --
  | JSContinue String
  -- |
  -- Raw Javascript (for an inline foreign import declarations)
  --
  | JSRaw String --deriving (Show, Eq, Data, Typeable)
