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
  (==) Negate     Negate     = true
  (==) Not        Not        = true
  (==) BitwiseNot BitwiseNot = true
  (==) Positive   Positive   = true
  (==) _ _ = false
  (/=) x y = not (x == y)
  
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
  | ZeroFillShiftRight --deriving (Show, Eq, Data, Typeable)

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
