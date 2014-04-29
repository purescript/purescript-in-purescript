module Language.PureScript.Values where

import Prelude
import Data.Array (concat)
import Data.Either
import Data.Maybe
import Data.Tuple
import Language.PureScript.Types
import Language.PureScript.Names

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard = Value

-- |
-- Data type for values
--
data Value
  -- |
  -- A numeric literal
  --
  = NumericLiteral Number
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | BinaryNoParens (Qualified Ident) Value Value
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | Parens Value
  -- |
  -- An array literal
  --
  | ArrayLiteral [Value]
  -- |
  -- An object literal
  --
  | ObjectLiteral [Tuple String Value]
  -- |
  -- An record property accessor expression
  --
  | Accessor String Value
  -- |
  -- Partial record update
  --
  | ObjectUpdate Value [Tuple String Value]
  -- |
  -- Function introduction
  --
  | Abs (Either Ident Binder) Value
  -- |
  -- Function application
  --
  | App Value Value
  -- |
  -- Variable
  --
  | Var (Qualified Ident)
  -- |
  -- Conditional (if-then-else expression)
  --
  | IfThenElse Value Value Value
  -- |
  -- A data constructor
  --
  | Constructor (Qualified ProperName)
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case [Value] [CaseAlternative]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Boolean Value Type
  -- |
  -- A let binding
  --
  | Let (Either Binder (Tuple Ident [Either Ident Binder])) Value Value
  -- |
  -- A do-notation block
  --
  | Do [DoNotationElement]
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary (Tuple (Qualified ProperName) [Type]) [TypeClassDictionaryInScope]
  
-- |
-- An alternative in a case statement
--
data CaseAlternative = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder]
    -- |
    -- An optional guard
    --
  , caseAlternativeGuard :: Maybe Guard
    -- |
    -- The result expression
    --
  , caseAlternativeResult :: Value
  }
  
-- |
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope = TypeClassDictionaryInScope 
  { -- |
    -- The identifier with which the dictionary can be accessed at runtime
    --
    tcdName :: Qualified Ident
    -- |
    -- The name of the type class to which this type class instance applies
    --
  , tcdClassName :: Qualified ProperName
    -- |
    -- The types to which this type class instance applies
    --
  , tcdInstanceTypes :: [Type]
    -- |
    -- Type class dependencies which must be satisfied to construct this dictionary
    --
  , tcdDependencies :: Maybe [Tuple (Qualified ProperName) [Type]]
    -- |
    -- The type of this dictionary
    --
  , tcdType :: TypeClassDictionaryType
  }
  
-- |
-- The type of a type class dictionary
--
data TypeClassDictionaryType
  -- |
  -- A regular type class dictionary
  --
  = TCDRegular
  -- |
  -- A type class dictionary which is an alias for an imported dictionary from another module
  --
  | TCDAlias (Qualified Ident)
  
-- |
-- Find the original dictionary which a type class dictionary in scope refers to
--
canonicalizeDictionary :: TypeClassDictionaryInScope -> Qualified Ident
canonicalizeDictionary (TypeClassDictionaryInScope { tcdType = TCDRegular, tcdName = nm }) = nm
canonicalizeDictionary (TypeClassDictionaryInScope { tcdType = TCDAlias nm }) = nm

-- |
-- A statement in a do-notation block
--
data DoNotationElement
  -- |
  -- A monadic value without a binder
  --
  = DoNotationValue Value
  -- |
  -- A monadic value with a binder
  --
  | DoNotationBind Binder Value
  -- |
  -- A let statement, i.e. a pure value with a binder
  --
  | DoNotationLet Binder Value
  
-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a boolean literal
  --
  | BooleanBinder Boolean
  -- |
  -- A binder which matches a string literal
  --
  | StringBinder String
  -- |
  -- A binder which matches a numeric literal
  --
  | NumberBinder Number
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified ProperName) [Binder]
  -- |
  -- A binder which matches a record and binds its properties
  --
  | ObjectBinder [Tuple String Binder]
  -- |
  -- A binder which matches an array and binds its elements
  --
  | ArrayBinder [Binder]
  -- |
  -- A binder which matches an array and binds its head and tail
  --
  | ConsBinder Binder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident Binder
  
