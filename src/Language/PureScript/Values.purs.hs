module Language.PureScript.Values where

import Prelude
import Data.Array (concat)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Generics
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
  
instance showValue :: Show Value where
  show = gshow
  
instance genericValue :: Generic Value where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.Value", args: [] }
  term (NumericLiteral n)          = TmCon { con: "Language.PureScript.Values.NumericLiteral"      , values: [term n] }
  term (StringLiteral s)           = TmCon { con: "Language.PureScript.Values.StringLiteral"       , values: [term s] }
  term (BooleanLiteral b)          = TmCon { con: "Language.PureScript.Values.BooleanLiteral"      , values: [term b] }
  term (BinaryNoParens name v1 v2) = TmCon { con: "Language.PureScript.Values.BinaryNoParens"      , values: [term name, term v1, term v2] }
  term (Parens v)                  = TmCon { con: "Language.PureScript.Values.Parens"              , values: [term v] }
  term (ArrayLiteral vs)           = TmCon { con: "Language.PureScript.Values.ArrayLiteral"        , values: [term vs] }
  term (ObjectLiteral props)       = TmCon { con: "Language.PureScript.Values.ObjectLiteral"       , values: [term props] }
  term (Accessor prop obj)         = TmCon { con: "Language.PureScript.Values.Accessor"            , values: [term prop, term obj] }
  term (ObjectUpdate obj props)    = TmCon { con: "Language.PureScript.Values.ObjectUpdate"        , values: [term obj, term props] }
  term (Abs arg v)                 = TmCon { con: "Language.PureScript.Values.Abs"                 , values: [term arg, term v] }
  term (App v1 v2)                 = TmCon { con: "Language.PureScript.Values.App"                 , values: [term v1, term v2] }
  term (Var name)                  = TmCon { con: "Language.PureScript.Values.Var"                 , values: [term name] }
  term (IfThenElse test th el)     = TmCon { con: "Language.PureScript.Values.IfThenElse"          , values: [term test, term th, term el] }
  term (Constructor name)          = TmCon { con: "Language.PureScript.Values.Constructor"         , values: [term name] }
  term (Case vs cas)               = TmCon { con: "Language.PureScript.Values.Case"                , values: [term vs, term cas] }
  term (TypedValue b v t)          = TmCon { con: "Language.PureScript.Values.TypedValue"          , values: [term b, term v, term t] }
  term (Let b v1 v2)               = TmCon { con: "Language.PureScript.Values.Let"                 , values: [term b, term v1, term v2] }
  term (Do sts)                    = TmCon { con: "Language.PureScript.Values.Do"                  , values: [term sts] }
  term (TypeClassDictionary tc ds) = TmCon { con: "Language.PureScript.Values.TypeClassDictionary" , values: [term tc, term ds] }
  unTerm (TmCon { con = "Language.PureScript.Values.NumericLiteral"      , values = [n]            }) = NumericLiteral      <$> unTerm n
  unTerm (TmCon { con = "Language.PureScript.Values.StringLiteral"       , values = [s]            }) = StringLiteral       <$> unTerm s
  unTerm (TmCon { con = "Language.PureScript.Values.BooleanLiteral"      , values = [b]            }) = BooleanLiteral      <$> unTerm b
  unTerm (TmCon { con = "Language.PureScript.Values.BinaryNoParens"      , values = [name, v1, v2] }) = BinaryNoParens      <$> unTerm name <*> unTerm v1 <*> unTerm v2
  unTerm (TmCon { con = "Language.PureScript.Values.Parens"              , values = [v]            }) = Parens              <$> unTerm v
  unTerm (TmCon { con = "Language.PureScript.Values.ArrayLiteral"        , values = [vs]           }) = ArrayLiteral        <$> unTerm vs
  unTerm (TmCon { con = "Language.PureScript.Values.ObjectLiteral"       , values = [props]        }) = ObjectLiteral       <$> unTerm props
  unTerm (TmCon { con = "Language.PureScript.Values.Accessor"            , values = [prop, obj]    }) = Accessor            <$> unTerm prop <*> unTerm obj
  unTerm (TmCon { con = "Language.PureScript.Values.ObjectUpdate"        , values = [obj, props]   }) = ObjectUpdate        <$> unTerm obj <*> unTerm props
  unTerm (TmCon { con = "Language.PureScript.Values.Abs"                 , values = [arg, v]       }) = Abs                 <$> unTerm arg <*> unTerm v
  unTerm (TmCon { con = "Language.PureScript.Values.App"                 , values = [v1, v2]       }) = App                 <$> unTerm v1 <*> unTerm v2
  unTerm (TmCon { con = "Language.PureScript.Values.Var"                 , values = [name]         }) = Var                 <$> unTerm name
  unTerm (TmCon { con = "Language.PureScript.Values.IfThenElse"          , values = [test, th, el] }) = IfThenElse          <$> unTerm test <*> unTerm th <*> unTerm el
  unTerm (TmCon { con = "Language.PureScript.Values.Constructor"         , values = [name]         }) = Constructor         <$> unTerm name
  unTerm (TmCon { con = "Language.PureScript.Values.Case"                , values = [vs, cas]      }) = Case                <$> unTerm vs <*> unTerm cas
  unTerm (TmCon { con = "Language.PureScript.Values.TypedValue"          , values = [b, v, t]      }) = TypedValue          <$> unTerm b <*> unTerm v <*> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Values.Let"                 , values = [b, v1, v2]    }) = Let                 <$> unTerm b <*> unTerm v1 <*> unTerm v2
  unTerm (TmCon { con = "Language.PureScript.Values.Do"                  , values = [sts]          }) = Do                  <$> unTerm sts
  unTerm (TmCon { con = "Language.PureScript.Values.TypeClassDictionary" , values = [tc, ds]       }) = TypeClassDictionary <$> unTerm tc <*> unTerm ds
  unTerm _ = Nothing

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
  
instance showCaseAlternative :: Show CaseAlternative where
  show = gshow

instance genericCaseAlternative :: Generic CaseAlternative where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.CaseAlternative", args: [] }
  term (CaseAlternative ca) =
    TmCon { con: "Language.PureScript.Values.CaseAlternative"
          , values: [ term ca.caseAlternativeBinders
                    , term ca.caseAlternativeGuard
                    , term ca.caseAlternativeResult
                    ] }
  unTerm (TmCon { con = "Language.PureScript.Values.CaseAlternative" , values = [cab, cag, car] }) =
    do
      caseAlternativeBinders <- unTerm cab
      caseAlternativeGuard   <- unTerm cag
      caseAlternativeResult  <- unTerm car
      return $ CaseAlternative
        { caseAlternativeBinders : caseAlternativeBinders
        , caseAlternativeGuard   : caseAlternativeGuard
        , caseAlternativeResult  : caseAlternativeResult
        }
  unTerm _ = Nothing

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
  
instance showTypeClassDictionaryInScope :: Show TypeClassDictionaryInScope where
  show = gshow

instance genericTypeClassDictionaryInScope :: Generic TypeClassDictionaryInScope where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.TypeClassDictionaryInScope", args: [] }
  term (TypeClassDictionaryInScope d) =
    TmCon { con: "Language.PureScript.Values.TypeClassDictionaryInScope"
          , values: [ term d.tcdName
                    , term d.tcdClassName
                    , term d.tcdInstanceTypes
                    , term d.tcdDependencies
                    , term d.tcdType
                    ] }
  unTerm (TmCon { con = "Language.PureScript.Values.TypeClassDictionaryInScope"
                , values = [tcdName, tcdClassName, tcdInstanceTypes, tcdDependencies, tcdType] }) =
    do
      tcdNameVal          <- unTerm tcdName
      tcdClassNameVal     <- unTerm tcdClassName
      tcdInstanceTypesVal <- unTerm tcdInstanceTypes
      tcdDependenciesVal  <- unTerm tcdDependencies
      tcdTypeVal          <- unTerm tcdType
      return $ TypeClassDictionaryInScope
        { tcdName:          tcdNameVal
        , tcdClassName:     tcdClassNameVal
        , tcdInstanceTypes: tcdInstanceTypesVal
        , tcdDependencies:  tcdDependenciesVal
        , tcdType:          tcdTypeVal
        }
  unTerm _ = Nothing

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
  
instance showTypeClassDictionaryType :: Show TypeClassDictionaryType where
  show = gshow
  
instance eqTypeClassDictionaryType :: Eq TypeClassDictionaryType where
  (==) = geq
  (/=) x y = not (x == y)

instance genericTypeClassDictionaryType :: Generic TypeClassDictionaryType where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.TypeClassDictionaryType", args: [] }
  term TCDRegular   = TmCon { con: "Language.PureScript.Values.TCDRegular" , values: [] }
  term (TCDAlias i) = TmCon { con: "Language.PureScript.Values.TCDAlias"   , values: [term i] }
  unTerm (TmCon { con = "Language.PureScript.Values.TCDRegular"              }) = Just TCDRegular
  unTerm (TmCon { con = "Language.PureScript.Values.TCDAlias" , values = [i] }) = TCDAlias <$> unTerm i
  unTerm _ = Nothing

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
  
instance showDoNotationElement :: Show DoNotationElement where
  show = gshow

instance genericDoNotationElement :: Generic DoNotationElement where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.DoNotationElement", args: [] }
  term (DoNotationValue v)  = TmCon { con: "Language.PureScript.Values.DoNotationValue" , values: [] }
  term (DoNotationBind b v) = TmCon { con: "Language.PureScript.Values.DoNotationBind"  , values: [term b, term v] }
  term (DoNotationLet b v)  = TmCon { con: "Language.PureScript.Values.DoNotationLet"   , values: [term b, term v] }
  unTerm (TmCon { con = "Language.PureScript.Values.DoNotationValue" , values = [v] })    = DoNotationValue <$> unTerm v
  unTerm (TmCon { con = "Language.PureScript.Values.DoNotationBind"  , values = [b, v] }) = DoNotationBind  <$> unTerm b <*> unTerm v
  unTerm (TmCon { con = "Language.PureScript.Values.DoNotationLet"   , values = [b, v] }) = DoNotationLet   <$> unTerm b <*> unTerm v
  unTerm _ = Nothing

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
  
instance showBinder :: Show Binder where
  show = gshow

instance genericBinder :: Generic Binder where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Values.Binder", args: [] }
  term NullBinder                  = TmCon { con: "Language.PureScript.Values.NullBinder"        , values: [] }
  term (BooleanBinder b)           = TmCon { con: "Language.PureScript.Values.BooleanBinder"     , values: [term b] }
  term (StringBinder s)            = TmCon { con: "Language.PureScript.Values.StringBinder"      , values: [term s] }
  term (NumberBinder n)            = TmCon { con: "Language.PureScript.Values.NumberBinder"      , values: [term n] }
  term (VarBinder i)               = TmCon { con: "Language.PureScript.Values.VarBinder"         , values: [term i] }
  term (ConstructorBinder ctor bs) = TmCon { con: "Language.PureScript.Values.ConstructorBinder" , values: [term ctor, term bs] }
  term (ObjectBinder es)           = TmCon { con: "Language.PureScript.Values.ObjectBinder"      , values: [term es] }
  term (ArrayBinder xs)            = TmCon { con: "Language.PureScript.Values.ArrayBinder"       , values: [term xs] }
  term (ConsBinder h t)            = TmCon { con: "Language.PureScript.Values.ConsBinder"        , values: [term h, term t] }
  term (NamedBinder i b)           = TmCon { con: "Language.PureScript.Values.NamedBinder"       , values: [term i, term b] }
  unTerm (TmCon { con = "Language.PureScript.Values.NullBinder"        , values = [] })         = Just NullBinder
  unTerm (TmCon { con = "Language.PureScript.Values.BooleanBinder"     , values = [b] })        = BooleanBinder     <$> unTerm b
  unTerm (TmCon { con = "Language.PureScript.Values.StringBinder"      , values = [s] })        = StringBinder      <$> unTerm s
  unTerm (TmCon { con = "Language.PureScript.Values.NumberBinder"      , values = [n] })        = NumberBinder      <$> unTerm n
  unTerm (TmCon { con = "Language.PureScript.Values.VarBinder"         , values = [i] })        = VarBinder         <$> unTerm i
  unTerm (TmCon { con = "Language.PureScript.Values.ConstructorBinder" , values = [ctor, bs] }) = ConstructorBinder <$> unTerm ctor <*> unTerm bs
  unTerm (TmCon { con = "Language.PureScript.Values.ObjectBinder"      , values = [es] })       = ObjectBinder      <$> unTerm es
  unTerm (TmCon { con = "Language.PureScript.Values.ArrayBinder"       , values = [xs] })       = ArrayBinder       <$> unTerm xs
  unTerm (TmCon { con = "Language.PureScript.Values.ConsBinder"        , values = [h, t] })     = ConsBinder        <$> unTerm h <*> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Values.NamedBinder"       , values = [i, b] })     = NamedBinder       <$> unTerm i <*> unTerm b
  unTerm _ = Nothing

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: forall d. (Generic d) => d -> [Ident]
binderNames = everything concat (mkQ [] binderName)

binderName (VarBinder ident)     = [ident]
binderName (NamedBinder ident _) = [ident]
binderName _                     = []
