module Language.PureScript.CodeGen.JS.AST where

import Data.Either
import Data.Maybe
import Data.Tuple

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
  show Negate = "Negate"
  show Not = "Not"
  show BitwiseNot = "BitwiseNot"
  show Positive = "Positive"
  
instance eqUnaryOperator :: Eq UnaryOperator where
  (==) Negate     Negate     = true
  (==) Not        Not        = true
  (==) BitwiseNot BitwiseNot = true
  (==) Positive   Positive   = true
  (==) _          _          = false
  (/=) u1         u2         = not (u1 == u2)

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
  show Add                   = "Add"
  show Subtract              = "Subtract"
  show Multiply              = "Multiply"
  show Divide                = "Divide"
  show Modulus               = "Modulus"
  show EqualTo               = "EqualTo"
  show NotEqualTo            = "NotEqualTo"
  show LessThan              = "LessThan"
  show LessThanOrEqualTo     = "LessThanOrEqualTo"
  show GreaterThan           = "GreaterThan"
  show GreaterThanOrEqualTo  = "GreaterThanOrEqualTo"
  show And                   = "And"
  show Or                    = "Or"
  show BitwiseAnd            = "BitwiseAnd"
  show BitwiseOr             = "BitwiseOr"
  show BitwiseXor            = "BitwiseXor"
  show ShiftLeft             = "ShiftLeft"
  show ShiftRight            = "ShiftRight"
  show ZeroFillShiftRight    = "ZeroFillShiftRight"

instance eqBinaryOperator :: Eq BinaryOperator where
  (==) Add                  Add                  = true
  (==) Subtract             Subtract             = true
  (==) Multiply             Multiply             = true
  (==) Divide               Divide               = true
  (==) Modulus              Modulus              = true
  (==) EqualTo              EqualTo              = true
  (==) NotEqualTo           NotEqualTo           = true
  (==) LessThan             LessThan             = true
  (==) LessThanOrEqualTo    LessThanOrEqualTo    = true
  (==) GreaterThan          GreaterThan          = true
  (==) GreaterThanOrEqualTo GreaterThanOrEqualTo = true
  (==) And                  And                  = true
  (==) Or                   Or                   = true
  (==) BitwiseAnd           BitwiseAnd           = true
  (==) BitwiseOr            BitwiseOr            = true
  (==) BitwiseXor           BitwiseXor           = true
  (==) ShiftLeft            ShiftLeft            = true
  (==) ShiftRight           ShiftRight           = true
  (==) ZeroFillShiftRight   ZeroFillShiftRight   = true
  (==) _                    _                    = false
  (/=) b1                   b2                   = not (b1 == b2)
  

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
  | JSRaw String
  
instance showJS :: Show JS where
  show (JSNumericLiteral n) = "JSNumericLiteral (" ++ show n ++ ")"
  show (JSStringLiteral s) = "JSStringLiteral (" ++ show s ++ ")"
  show (JSBooleanLiteral b) = "JSBooleanLiteral (" ++ show b ++ ")"
  show (JSUnary op js) = "JSUnary (" ++ show op ++ ") (" ++ show js ++ ")"
  show (JSBinary op js1 js2) = "JSBinary (" ++ show op ++ ") (" ++ show js1 ++ ") (" ++ show js2 ++ ")"
  show (JSArrayLiteral js) = "JSArrayLiteral (" ++ show js ++ ")"
  show (JSIndexer js1 js2) = "JSIndexer (" ++ show js1 ++ ") (" ++ show js2 ++ ")"
  show (JSObjectLiteral ps) = "JSObjectLiteral (" ++ show ps ++ ")"
  show (JSAccessor prop js) = "JSAccessor (" ++ show prop ++ ") (" ++ show js ++ ")"
  show (JSFunction nm args js) = "JSFunction (" ++ show nm ++ ") (" ++ show args ++ ") (" ++ show js ++ ")"
  show (JSApp js args) = "JSApp (" ++ show js ++ ") (" ++ show args ++ ")"
  show (JSVar nm) = "JSVar (" ++ show nm ++ ")"
  show (JSConditional js1 js2 js3) = "JSConditional (" ++ show js1 ++ ") (" ++ show js2 ++ ") (" ++ show js3 ++ ")"
  show (JSBlock js) = "JSBlock (" ++ show js ++ ")"
  show (JSVariableIntroduction nm js) = "JSVariableIntroduction (" ++ show nm ++ ") (" ++ show js ++ ")"
  show (JSAssignment js1 js2) = "JSAssignment (" ++ show js1 ++ ") (" ++ show js2 ++ ")"
  show (JSWhile js1 js2) = "JSWhile (" ++ show js1 ++ ") (" ++ show js2 ++ ")"
  show (JSFor nm js1 js2 js3) = "JSFor (" ++ show nm ++ ") (" ++ show js1 ++ ") (" ++ show js2 ++ ") (" ++ show js3 ++ ")"
  show (JSForIn nm js1 js2) = "JSForIn String (" ++ show nm ++ ") (" ++ show js1 ++ ") (" ++ show js2 ++ ")"
  show (JSIfElse js1 js2 js3) = "JSIfElse (" ++ show js1 ++ ") (" ++ show js2 ++ ") (" ++ show js3 ++ ")"
  show (JSReturn js) = "JSReturn (" ++ show js ++ ")"
  show (JSThrow js) = "JSThrow (" ++ show js ++ ")"
  show (JSTypeOf js) = "JSTypeOf (" ++ show js ++ ")"
  show (JSLabel lbl js) = "JSLabel (" ++ show lbl ++ ") (" ++ show js ++ ")"
  show (JSBreak lbl) = "JSBreak (" ++ show lbl ++ ")"
  show (JSContinue lbl) = "JSContinue (" ++ show lbl ++ ")"
  show (JSRaw js) = "JSRaw (" ++ show js ++ ")"
