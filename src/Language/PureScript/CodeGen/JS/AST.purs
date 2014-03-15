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
  | JSRaw String

instance showJS :: Show JS where
  show = gshow

instance eqJS :: Eq JS where
  (==) = geq
  (/=) x y = not (geq x y)

instance genericJS :: Generic JS where
  typeOf _ = TyCon { tyCon: "Language.PureScript.CodeGen.JS.AST.BinaryOperator", args: [] }
  term (JSNumericLiteral n)              = TmCon { con: "Language.PureScript.Names.JSNumericLiteral"       , values: [term n] }
  term (JSStringLiteral s)               = TmCon { con: "Language.PureScript.Names.JSStringLiteral"        , values: [term s] }
  term (JSBooleanLiteral b)              = TmCon { con: "Language.PureScript.Names.JSBooleanLiteral"       , values: [term b] }
  term (JSUnary op x)                    = TmCon { con: "Language.PureScript.Names.JSUnary"                , values: [term op, term x] }
  term (JSBinary op x y)                 = TmCon { con: "Language.PureScript.Names.JSBinary"               , values: [term op, term x, term y] }
  term (JSArrayLiteral es)               = TmCon { con: "Language.PureScript.Names.JSArrayLiteral"         , values: [term es] }
  term (JSIndexer prop val)              = TmCon { con: "Language.PureScript.Names.JSIndexer"              , values: [term prop, term val] }
  term (JSObjectLiteral props)           = TmCon { con: "Language.PureScript.Names.JSObjectLiteral"        , values: [term props] }
  term (JSAccessor prop val)             = TmCon { con: "Language.PureScript.Names.JSAccessor"             , values: [term prop, term val] }
  term (JSFunction name args body)       = TmCon { con: "Language.PureScript.Names.JSFunction"             , values: [term name, term args, term body] }
  term (JSApp fn args)                   = TmCon { con: "Language.PureScript.Names.JSApp"                  , values: [term fn, term args] }
  term (JSVar name)                      = TmCon { con: "Language.PureScript.Names.JSVar"                  , values: [term name] }
  term (JSConditional cond th el)        = TmCon { con: "Language.PureScript.Names.JSConditional"          , values: [term cond, term th, term el] }
  term (JSBlock sts)                     = TmCon { con: "Language.PureScript.Names.JSBlock"                , values: [term sts] }
  term (JSVariableIntroduction name val) = TmCon { con: "Language.PureScript.Names.JSVariableIntroduction" , values: [term name, term val] }
  term (JSAssignment targ val)           = TmCon { con: "Language.PureScript.Names.JSAssignment"           , values: [term targ, term val] }
  term (JSWhile cond sts)                = TmCon { con: "Language.PureScript.Names.JSWhile"                , values: [term cond, term sts] }
  term (JSFor name start end sts)        = TmCon { con: "Language.PureScript.Names.JSFor"                  , values: [term name, term start, term end, term sts] }
  term (JSForIn name obj sts)            = TmCon { con: "Language.PureScript.Names.JSForIn"                , values: [term name, term obj, term sts] }
  term (JSIfElse cond ths els)           = TmCon { con: "Language.PureScript.Names.JSIfElse"               , values: [term cond, term ths, term els] }
  term (JSReturn val)                    = TmCon { con: "Language.PureScript.Names.JSReturn"               , values: [term val] }
  term (JSThrow val)                     = TmCon { con: "Language.PureScript.Names.JSThrow"                , values: [term val] }
  term (JSTypeOf val)                    = TmCon { con: "Language.PureScript.Names.JSTypeOf"               , values: [term val] }
  term (JSLabel name st)                 = TmCon { con: "Language.PureScript.Names.JSLabel"                , values: [term name, term st] }
  term (JSBreak name)                    = TmCon { con: "Language.PureScript.Names.JSBreak"                , values: [term name] }
  term (JSContinue name)                 = TmCon { con: "Language.PureScript.Names.JSContinue"             , values: [term name] }
  term (JSRaw js)                        = TmCon { con: "Language.PureScript.Names.JSRaw"                  , values: [term js] }
  unTerm (TmCon { con = "Language.PureScript.Names.JSNumericLiteral"       , values = [n]                      }) = JSNumericLiteral        <$> unTerm n
  unTerm (TmCon { con = "Language.PureScript.Names.JSStringLiteral"        , values = [s]                      }) = JSStringLiteral         <$> unTerm s
  unTerm (TmCon { con = "Language.PureScript.Names.JSBooleanLiteral"       , values = [b]                      }) = JSBooleanLiteral        <$> unTerm b
  unTerm (TmCon { con = "Language.PureScript.Names.JSUnary"                , values = [op, x]                  }) = JSUnary                 <$> unTerm op <*> unTerm x
  unTerm (TmCon { con = "Language.PureScript.Names.JSBinary"               , values = [op, x, y]               }) = JSBinary                <$> unTerm op <*> unTerm x <*> unTerm y
  unTerm (TmCon { con = "Language.PureScript.Names.JSArrayLiteral"         , values = [es]                     }) = JSArrayLiteral          <$> unTerm es
  unTerm (TmCon { con = "Language.PureScript.Names.JSIndexer"              , values = [prop, val]              }) = JSIndexer               <$> unTerm prop <*> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSObjectLiteral"        , values = [props]                  }) = JSObjectLiteral         <$> unTerm props
  unTerm (TmCon { con = "Language.PureScript.Names.JSAccessor"             , values = [prop, val]              }) = JSAccessor              <$> unTerm prop <*> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSFunction"             , values = [name, args, body]       }) = JSFunction              <$> unTerm name <*> unTerm args <*> unTerm body
  unTerm (TmCon { con = "Language.PureScript.Names.JSApp"                  , values = [fn, args]               }) = JSApp                   <$> unTerm fn <*> unTerm args
  unTerm (TmCon { con = "Language.PureScript.Names.JSVar"                  , values = [name]                   }) = JSVar                   <$> unTerm name
  unTerm (TmCon { con = "Language.PureScript.Names.JSConditional"          , values = [cond, th, el]           }) = JSConditional           <$> unTerm cond <*> unTerm th <*> unTerm el
  unTerm (TmCon { con = "Language.PureScript.Names.JSBlock"                , values = [sts]                    }) = JSBlock                 <$> unTerm sts
  unTerm (TmCon { con = "Language.PureScript.Names.JSVariableIntroduction" , values = [name, val]              }) = JSVariableIntroduction  <$> unTerm name <*> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSAssignment"           , values = [targ, val]              }) = JSAssignment            <$> unTerm targ <*> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSWhile"                , values = [cond, sts]              }) = JSWhile                 <$> unTerm cond <*> unTerm sts
  unTerm (TmCon { con = "Language.PureScript.Names.JSFor"                  , values = [name, start, end, sts]  }) = JSFor                   <$> unTerm name <*> unTerm start <*> unTerm end <*> unTerm sts
  unTerm (TmCon { con = "Language.PureScript.Names.JSForIn"                , values = [name, obj, sts]         }) = JSForIn                 <$> unTerm name <*> unTerm obj <*> unTerm sts
  unTerm (TmCon { con = "Language.PureScript.Names.JSIfElse"               , values = [cond, ths, els]         }) = JSIfElse                <$> unTerm cond <*> unTerm ths <*> unTerm els
  unTerm (TmCon { con = "Language.PureScript.Names.JSReturn"               , values = [val]                    }) = JSReturn                <$> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSThrow"                , values = [val]                    }) = JSThrow                 <$> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSTypeOf"               , values = [val]                    }) = JSTypeOf                <$> unTerm val
  unTerm (TmCon { con = "Language.PureScript.Names.JSLabel"                , values = [name, st]               }) = JSLabel                 <$> unTerm name <*> unTerm st
  unTerm (TmCon { con = "Language.PureScript.Names.JSBreak"                , values = [name]                   }) = JSBreak                 <$> unTerm name
  unTerm (TmCon { con = "Language.PureScript.Names.JSContinue"             , values = [name]                   }) = JSContinue              <$> unTerm name
  unTerm (TmCon { con = "Language.PureScript.Names.JSRaw"                  , values = [js]                     }) = JSRaw                   <$> unTerm js
  unTerm _ = Nothing
