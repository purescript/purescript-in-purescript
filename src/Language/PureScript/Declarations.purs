module Language.PureScript.Declarations where

import Control.Apply
import Control.Bind
import Data.Array (concatMap, map)
import Data.Either
import Data.Foldable (foldl, mconcat)
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)
import Data.Tuple
import Data.Tuple3

import Language.PureScript.Pos
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.Types
import Language.PureScript.TypeClassDictionaries

-- |
-- A precedence level for an infix operator
--
type Precedence = Number

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr | Infix

instance showAssociativity :: Show Associativity where
  show Infixl = "infixl"
  show Infixr = "infixr"
  show Infix  = "infix"

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence

instance showFixity :: Show Fixity where
  show (Fixity assoc prec) = "Fixity (" ++ show assoc ++ ") (" ++ show prec ++ ")"

-- |
-- A module declaration, consisting of a module name, a list of declarations, and a list of the
-- declarations that are explicitly exported. If the export list is Nothing, everything is exported.
--
data Module = Module ModuleName [Declaration] (Maybe [DeclarationRef])

instance showModule :: Show Module where
  show (Module nm ds rs) = "Module (" ++ show nm ++ ") (" ++ show ds ++ ") (" ++ show rs ++ ")"

-- |
-- An item in a list of explicit imports or exports
--
data DeclarationRef
  -- |
  -- A type constructor with data constructors
  --
  = TypeRef ProperName (Maybe [ProperName])
  -- |
  -- A value
  --
  | ValueRef Ident
  -- |
  -- A type class
  --
  | TypeClassRef ProperName
    -- |
  -- A type class instance, created during typeclass desugaring (name, class name, instance types)
  --
  | TypeInstanceRef Ident
  -- |
  -- A declaration reference with source position information
  --
  | PositionedDeclarationRef SourcePos DeclarationRef

instance showDeclarationRef :: Show DeclarationRef where
  show (TypeRef pn dctors) = "TypeRef (" ++ show pn ++ ") (" ++ show dctors ++ ")"
  show (ValueRef nm) = "ValueRef (" ++ show nm ++ ")"
  show (TypeClassRef nm) = "TypeClassRef (" ++ show nm ++ ")"
  show (TypeInstanceRef nm) = "TypeInstanceRef (" ++ show nm ++ ")"
  show (PositionedDeclarationRef pos ref) = "PositionedDeclarationRef (" ++ show pos ++ ") (" ++ show ref ++ ")"

instance eqDeclarationRef :: Eq DeclarationRef where
  (==) (TypeRef name dctors)  (TypeRef name' dctors') = name == name' && dctors == dctors'
  (==) (ValueRef name)        (ValueRef name')        = name == name'
  (==) (TypeClassRef name)    (TypeClassRef name')    = name == name'
  (==) (TypeInstanceRef name) (TypeInstanceRef name') = name == name'
  (==) (PositionedDeclarationRef _ r) r' = r == r'
  (==) r (PositionedDeclarationRef _ r') = r == r'
  (==) _ _ = false
  (/=) x y = not (x == y)

-- |
-- The data type of declarations
--
data Declaration
  -- |
  -- A data type declaration (name, arguments, data constructors)
  --
  = DataDeclaration ProperName [String] [Tuple ProperName [Type]]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration [Declaration]
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration ProperName [String] Type
  -- |
  -- A type declaration for a value (name, ty)
  --
  | TypeDeclaration Ident Type
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
  | ValueDeclaration Ident NameKind [Binder] (Maybe Guard) Value
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration [Tuple3 Ident NameKind Value]
  -- |
  -- A foreign import declaration (type, name, optional inline Javascript, type)
  --
  | ExternDeclaration ForeignImportType Ident (Maybe JS) Type
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration ProperName Kind
  -- |
  -- A type class instance foreign import
  --
  | ExternInstanceDeclaration Ident [Tuple (Qualified ProperName) [Type]] (Qualified ProperName) [Type]
  -- |
  -- A fixity declaration (fixity data, operator name)
  --
  | FixityDeclaration Fixity String
  -- |
  -- A module import (module name, optional set of identifiers to import, optional "qualified as"
  -- name)
  --
  | ImportDeclaration ModuleName (Maybe [DeclarationRef]) (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration ProperName [String] [Tuple (Qualified ProperName) [Type]] [Declaration]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration Ident [Tuple (Qualified ProperName) [Type]] (Qualified ProperName) [Type] [Declaration]
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration SourcePos Declaration

instance showDeclaration :: Show Declaration where
  show (DataDeclaration nm args dctors) =                   "DataDeclaration (" ++ show nm ++ ") (" ++ show args ++ ") (" ++ show dctors ++ ")"
  show (DataBindingGroupDeclaration ds) =                   "DataBindingGroupDeclaration (" ++ show ds ++ ")"
  show (TypeSynonymDeclaration nm args ty) =                "TypeSynonymDeclaration (" ++ show nm ++ ") (" ++ show args ++ ") (" ++ show ty ++ ")"
  show (TypeDeclaration nm ty) =                            "TypeDeclaration (" ++ show nm ++ ") (" ++ show ty ++ ")"
  show (ValueDeclaration nm nameKind bs grd val) =          "ValueDeclaration (" ++ show nm ++ ") (" ++ show nameKind ++ ") (" ++ show bs ++ ") (" ++ show grd ++ ") (" ++ show val ++ ")"
  show (BindingGroupDeclaration ds) =                       "BindingGroupDeclaration (" ++ show ds ++ ")"
  show (ExternDeclaration impTy nm js ty) =                 "ExternDeclaration (" ++ show impTy ++ ") (" ++ show nm ++ ") (" ++ show js ++ ") (" ++ show ty ++ ")"
  show (ExternDataDeclaration nm k) =                       "ExternDataDeclaration (" ++ show nm ++ ") (" ++ show k ++ ")"
  show (ExternInstanceDeclaration nm cs className tys) =    "ExternInstanceDeclaration (" ++ show nm ++ ") (" ++ show cs ++ ") (" ++ show className ++ ") (" ++ show tys ++ ")"
  show (FixityDeclaration f nm) =                           "FixityDeclaration (" ++ show f ++ ") (" ++ show nm ++ ")"
  show (ImportDeclaration mn imps qual) =                   "ImportDeclaration (" ++ show mn ++ ") (" ++ show imps ++ ") (" ++ show qual ++ ")"
  show (TypeClassDeclaration className args cs ds) =        "TypeClassDeclaration (" ++ show className ++ ") (" ++ show args ++ ") (" ++ show cs ++ ") (" ++ show ds ++ ")"
  show (TypeInstanceDeclaration nm cs className tys ds) =   "TypeInstanceDeclaration (" ++ show nm ++ ") (" ++ show cs ++ ") (" ++ show className ++ ") (" ++ show tys ++ ") (" ++ show ds ++ ")"
  show (PositionedDeclaration pos d) =                      "PositionedDeclaration (" ++ show pos ++ ") (" ++ show d ++ ")"

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration -> Boolean
isValueDecl (ValueDeclaration _ _ _ _ _) = true
isValueDecl (PositionedDeclaration _ d) = isValueDecl d
isValueDecl _ = false

-- |
-- Test if a declaration is a data type or type synonym declaration
--
isDataDecl :: Declaration -> Boolean
isDataDecl (DataDeclaration _ _ _) = true
isDataDecl (TypeSynonymDeclaration _ _ _) = true
isDataDecl (PositionedDeclaration _ d) = isDataDecl d
isDataDecl _ = false

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration -> Boolean
isImportDecl (ImportDeclaration _ _ _) = true
isImportDecl (PositionedDeclaration _ d) = isImportDecl d
isImportDecl _ = false

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration -> Boolean
isExternDataDecl (ExternDataDeclaration _ _) = true
isExternDataDecl (PositionedDeclaration _ d) = isExternDataDecl d
isExternDataDecl _ = false

-- |
-- Test if a declaration is a type class instance foreign import
--
isExternInstanceDecl :: Declaration -> Boolean
isExternInstanceDecl (ExternInstanceDeclaration _ _ _ _) = true
isExternInstanceDecl (PositionedDeclaration _ d) = isExternInstanceDecl d
isExternInstanceDecl _ = false

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration -> Boolean
isFixityDecl (FixityDeclaration _ _) = true
isFixityDecl (PositionedDeclaration _ d) = isFixityDecl d
isFixityDecl _ = false

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration -> Boolean
isExternDecl (ExternDeclaration _ _ _ _) = true
isExternDecl (PositionedDeclaration _ d) = isExternDecl d
isExternDecl _ = false

-- |
-- Test if a declaration is a type class or instance declaration
--
isTypeClassDeclaration :: Declaration -> Boolean
isTypeClassDeclaration (TypeClassDeclaration _ _ _ _) = true
isTypeClassDeclaration (TypeInstanceDeclaration _ _ _ _ _) = true
isTypeClassDeclaration (PositionedDeclaration _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = false

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
  -- A prefix -, will be desugared
  --
  | UnaryMinus Value
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
  | Let [Declaration] Value
  -- |
  -- A do-notation block
  --
  | Do [DoNotationElement]
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary Boolean (Tuple (Qualified ProperName) [Type]) [TypeClassDictionaryInScope]
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | SuperClassDictionary (Qualified ProperName) [Type]
  -- |
  -- A value with source position information
  --
  | PositionedValue SourcePos Value

instance showValue :: Show Value where
  show (NumericLiteral n) = "NumericLiteral (" ++ show n ++ ")"
  show (StringLiteral s) = "StringLiteral (" ++ show s ++ ")"
  show (BooleanLiteral b) = "BooleanLiteral (" ++ show b ++ ")"
  show (UnaryMinus v) = "UnaryMinus (" ++ show v ++ ")"
  show (BinaryNoParens op v1 v2) = "BinaryNoParens (" ++ show op ++ ") (" ++ show v1 ++ ") (" ++ show v2 ++ ")"
  show (Parens v) = "Parens (" ++ show v ++ ")"
  show (ArrayLiteral vs) = "ArrayLiteral (" ++ show vs ++ ")"
  show (ObjectLiteral ps) = "ObjectLiteral (" ++ show ps ++ ")"
  show (Accessor prop v) = "Accessor (" ++ show prop ++ ") (" ++ show v ++ ")"
  show (ObjectUpdate v ps) = "ObjectUpdate (" ++ show v ++ ") (" ++ show ps ++ ")"
  show (Abs arg v) = "Abs (" ++ show arg ++ ") (" ++ show v ++ ")"
  show (App v1 v2) = "App (" ++ show v1 ++ ") (" ++ show v2 ++ ")"
  show (Var id) = "Var (" ++ show id ++ ")"
  show (IfThenElse v1 v2 v3) = "IfThenElse (" ++ show v1 ++ ") (" ++ show v2 ++ ") (" ++ show v3 ++ ")"
  show (Constructor ctor) = "Constructor (" ++ show ctor ++ ")"
  show (Case val alts) = "Case (" ++ show val ++ ") (" ++ show alts ++ ")"
  show (TypedValue check val ty) = "TypedValue (" ++ show check ++ ") (" ++ show val ++ ") (" ++ show ty ++ ")"
  show (Let ds val) = "Let (" ++ show ds ++ ") (" ++ show val ++ ")"
  show (Do els) = "Do (" ++ show els ++ ")"
  show (TypeClassDictionary useSups cs sco) = "TypeClassDictionary (" ++ show useSups ++ ") (" ++ show cs ++ ") (" ++ show sco ++ ")"
  show (SuperClassDictionary className tys) = "SuperClassDictionary (" ++ show className ++ ") (" ++ show tys ++ ")"
  show (PositionedValue pos val) = "PositionedValue (" ++ show pos ++ ") (" ++ show val ++ ")"

-- |
-- An alternative in a case statement
--
data CaseAlternative = CaseAlternative { binders :: [Binder]
                                       , guard :: Maybe Guard
                                       , result :: Value }

mkCaseAlternative :: [Binder] -> Maybe Guard -> Value -> CaseAlternative
mkCaseAlternative bs g r = CaseAlternative { binders: bs, guard: g, result: r }

instance showCaseAlternative :: Show CaseAlternative where
  show (CaseAlternative o) = "CaseAlternative (" ++ show o.binders ++ ") (" ++ show o.guard ++ ") (" ++ show o.result ++ ")"

-- |
-- Find the original dictionary which a type class dictionary in scope refers to
--
canonicalizeDictionary :: TypeClassDictionaryInScope -> Qualified Ident
canonicalizeDictionary (TypeClassDictionaryInScope { ty = TCDRegular, name = nm }) = nm
canonicalizeDictionary (TypeClassDictionaryInScope { ty = TCDAlias nm }) = nm

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
  | DoNotationLet [Declaration]
  -- |
  -- A do notation element with source position information
  --
  | PositionedDoNotationElement SourcePos DoNotationElement

instance showDoNotationElement :: Show DoNotationElement where
  show (DoNotationValue val) = "DoNotationValue (" ++ show val ++ ")"
  show (DoNotationBind b val) = "DoNotationBind (" ++ show b ++ ") (" ++ show val ++ ")"
  show (DoNotationLet ds) = "DoNotationLet (" ++ show ds ++ ")"
  show (PositionedDoNotationElement pos e) = "PositionedDoNotationElement (" ++ show pos ++ ") (" ++ show e ++ ")"

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
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourcePos Binder

instance showBinder :: Show Binder where
  show NullBinder = "NullBinder"
  show (BooleanBinder b) = "BooleanBinder (" ++ show b ++ ")"
  show (StringBinder s) = "StringBinder (" ++ show s ++ ")"
  show (NumberBinder n) = "NumberBinder (" ++ show n ++ ")"
  show (VarBinder nm) = "VarBinder (" ++ show nm ++ ")"
  show (ConstructorBinder ctor bs) = "ConstructorBinder (" ++ show ctor ++ ") (" ++ show bs ++ ")"
  show (ObjectBinder ps) = "ObjectBinder (" ++ show ps ++ ")"
  show (ArrayBinder bs) = "ArrayBinder (" ++ show bs ++ ")"
  show (ConsBinder b1 b2) = "ConsBinder (" ++ show b1 ++ ") (" ++ show b2 ++ ")"
  show (NamedBinder nm b) = "NamedBinder (" ++ show nm ++ ") (" ++ show b ++ ")"
  show (PositionedBinder pos b) = "PositionedBinder (" ++ show pos ++ ") (" ++ show b ++ ")"


-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = go []
  where
  go ns (VarBinder name) = name : ns
  go ns (ConstructorBinder _ bs) = foldl go ns bs
  go ns (ObjectBinder bs) = foldl go ns (map snd bs)
  go ns (ArrayBinder bs) = foldl go ns bs
  go ns (ConsBinder b1 b2) = go (go ns b1) b2
  go ns (NamedBinder name b) = go (name : ns) b
  go ns (PositionedBinder _ b) = go ns b
  go ns _ = ns

--
-- Traversals
--

everywhereOnValues :: (Declaration -> Declaration) ->
                      (Value -> Value) ->
                      (Binder -> Binder) ->
                      { decls :: Declaration -> Declaration
                      , values :: Value -> Value
                      , binders :: Binder -> Binder }
everywhereOnValues f g h = { decls: f', values: g', binders: h' }
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration name nameKind bs grd val) = f (ValueDeclaration name nameKind (map h' bs) ((<$>) g' grd) (g' val))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (map (\(Tuple3 name nameKind val) -> Tuple3 name nameKind (g' val)) ds))
  f' (TypeClassDeclaration name args implies ds) = f (TypeClassDeclaration name args implies (map f' ds))
  f' (TypeInstanceDeclaration name cs className args ds) = f (TypeInstanceDeclaration name cs className args (map f' ds))
  f' (PositionedDeclaration pos d) = f (PositionedDeclaration pos (f' d))
  f' other = f other

  g' :: Value -> Value
  g' (UnaryMinus v) = g (UnaryMinus (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens op (g' v1) (g' v2))
  g' (Parens v) = g (Parens (g' v))
  g' (ArrayLiteral vs) = g (ArrayLiteral (map g' vs))
  g' (ObjectLiteral vs) = g (ObjectLiteral (map ((<$>) g') vs))
  g' (Accessor prop v) = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map ((<$>) g') vs))
  g' (Abs name v) = g (Abs name (g' v))
  g' (App v1 v2) = g (App (g' v1) (g' v2))
  g' (IfThenElse v1 v2 v3) = g (IfThenElse (g' v1) (g' v2) (g' v3))
  g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue check v ty) = g (TypedValue check (g' v) ty)
  g' (Let ds v) = g (Let (map f' ds) (g' v))
  g' (Do es) = g (Do (map handleDoNotationElement es))
  g' (PositionedValue pos v) = g (PositionedValue pos (g' v))
  g' other = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ctor bs) = h (ConstructorBinder ctor (map h' bs))
  h' (ObjectBinder bs) = h (ObjectBinder (map ((<$>) h') bs))
  h' (ArrayBinder bs) = h (ArrayBinder (map h' bs))
  h' (ConsBinder b1 b2) = h (ConsBinder (h' b1) (h' b2))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' (PositionedBinder pos b) = h (PositionedBinder pos (h' b))
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative (CaseAlternative ca) =
    CaseAlternative { binders: map h' ca.binders
                    , guard: g' <$> ca.guard
                    , result: g' ca.result }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos (handleDoNotationElement e)

everywhereOnValuesTopDownM :: forall m. (Monad m) =>
  (Declaration -> m Declaration) ->
  (Value -> m Value) ->
  (Binder -> m Binder) ->
  { decls :: Declaration -> m Declaration
  , values :: Value -> m Value
  , binders :: Binder -> m Binder }
everywhereOnValuesTopDownM f g h = { decls: f' <=< f, values: g' <=< g, binders: h' <=< h }
  where
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds
  f' (ValueDeclaration name nameKind bs grd val) = ValueDeclaration name nameKind <$> traverse (h' <=< h) bs <*> maybeM (g' <=< g) grd <*> (g val >>= g')
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (\(Tuple3 name nameKind val) -> Tuple3 name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f' <=< f) ds
  f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverse (f' <=< f) ds
  f' (PositionedDeclaration pos d) = PositionedDeclaration pos <$> (f d >>= f')
  f' other = f other

  g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) = BinaryNoParens op <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v) = Parens <$> (g v >>= g')
  g' (ArrayLiteral vs) = ArrayLiteral <$> traverse (g' <=< g) vs
  g' (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g' <=< g)) vs
  g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
  g' (Abs name v) = Abs name <$> (g v >>= g')
  g' (App v1 v2) = App <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts) = Case <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let ds v) = Let <$> traverse (f' <=< f) ds <*> (g v >>= g')
  g' (Do es) = Do <$> traverse handleDoNotationElement es
  g' (PositionedValue pos v) = PositionedValue pos <$> (g v >>= g')
  g' other = g other

  h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h' <=< h) bs
  h' (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h' <=< h)) bs
  h' (ArrayBinder bs) = ArrayBinder <$> traverse (h' <=< h) bs
  h' (ConsBinder b1 b2) = ConsBinder <$> (h b1 >>= h') <*> (h b2 >>= h')
  h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
  h' (PositionedBinder pos b) = PositionedBinder pos <$> (h b >>= h')
  h' other = h other

  handleCaseAlternative (CaseAlternative ca) =
    mkCaseAlternative <$> traverse (h' <=< h) ca.binders
                      <*> maybeM (g' <=< g) ca.guard
                      <*> (g' <=< g) ca.result

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse (f' <=< f) ds
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos <$> handleDoNotationElement e

everywhereOnValuesM :: forall m. (Monad m) =>
  (Declaration -> m Declaration) ->
  (Value -> m Value) ->
  (Binder -> m Binder) ->
  { decls :: Declaration -> m Declaration
  , values :: Value -> m Value
  , binders :: Binder -> m Binder }
everywhereOnValuesM f g h = { decls: f' <=< f, values: g' <=< g, binders: h' <=< h }
  where
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
  f' (ValueDeclaration name nameKind bs grd val) = (ValueDeclaration name nameKind <$> traverse h' bs <*> maybeM g' grd <*> g' val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> traverse (\(Tuple3 name nameKind val) -> Tuple3 name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration name args implies ds) = (TypeClassDeclaration name args implies <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> traverse f' ds) >>= f
  f' (PositionedDeclaration pos d) = (PositionedDeclaration pos <$> f' d) >>= f
  f' other = f other

  g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) = (BinaryNoParens op <$> (g' v1) <*> (g' v2)) >>= g
  g' (Parens v) = (Parens <$> g' v) >>= g
  g' (ArrayLiteral vs) = (ArrayLiteral <$> traverse g' vs) >>= g
  g' (ObjectLiteral vs) = (ObjectLiteral <$> traverse (sndM g') vs) >>= g
  g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> traverse (sndM g') vs) >>= g
  g' (Abs name v) = (Abs name <$> g' v) >>= g
  g' (App v1 v2) = (App <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts) = (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let ds v) = (Let <$> traverse f' ds <*> g' v) >>= g
  g' (Do es) = (Do <$> traverse handleDoNotationElement es) >>= g
  g' (PositionedValue pos v) = (PositionedValue pos <$> g' v) >>= g
  g' other = g other

  h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> traverse h' bs) >>= h
  h' (ObjectBinder bs) = (ObjectBinder <$> traverse (sndM h') bs) >>= h
  h' (ArrayBinder bs) = (ArrayBinder <$> traverse h' bs) >>= h
  h' (ConsBinder b1 b2) = (ConsBinder <$> h' b1 <*> h' b2) >>= h
  h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
  h' (PositionedBinder pos b) = (PositionedBinder pos <$> h' b) >>= h
  h' other = h other

  handleCaseAlternative (CaseAlternative ca) =
    mkCaseAlternative <$> traverse h' ca.binders
                      <*> maybeM g' ca.guard
                      <*> g' ca.result

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse f' ds
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos <$> handleDoNotationElement e


everythingOnValues :: forall r.
                      (r -> r -> r) ->
                      (Declaration -> r) ->
                      (Value -> r) ->
                      (Binder -> r) ->
                      (CaseAlternative -> r) ->
                      (DoNotationElement -> r) ->
                      { decls :: Declaration -> r
                      , values :: Value -> r
                      , binders :: Binder -> r
                      , cases :: CaseAlternative -> r
                      , dos :: DoNotationElement -> r }
everythingOnValues (<>) f g h i j = { decls: f'
                                    , values: g'
                                    , binders: h'
                                    , cases: i'
                                    , dos: j' }
  where
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs Nothing val) = foldl (<>) (f d) (map h' bs) <> g' val
  f' d@(ValueDeclaration _ _ bs (Just grd) val) = foldl (<>) (f d) (map h' bs) <> g' grd <> g' val
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (map (\(Tuple3 _ _ val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ d1) = f d <> f' d1
  f' d = f d

  g' v@(UnaryMinus v1) = g v <> g' v1
  g' v@(BinaryNoParens _ v1 v2) = g v <> g' v1 <> g' v2
  g' v@(Parens v1) = g v <> g' v1
  g' v@(ArrayLiteral vs) = foldl (<>) (g v) (map g' vs)
  g' v@(ObjectLiteral vs) = foldl (<>) (g v) (map (g' <<< snd) vs)
  g' v@(Accessor _ v1) = g v <> g' v1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' <<< snd) vs)
  g' v@(Abs _ v1) = g v <> g' v1
  g' v@(App v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _) = g v <> g' v1
  g' v@(Let ds v1) = (foldl (<>) (g v) (map f' ds)) <> g' v1
  g' v@(Do es) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ v1) = g v <> g' v1
  g' v = g v

  h' b@(ConstructorBinder _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ObjectBinder bs) = foldl (<>) (h b) (map (h' <<< snd) bs)
  h' b@(ArrayBinder bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ConsBinder b1 b2) = h b <> h' b1 <> h' b2
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ b1) = h b <> h' b1
  h' b = h b

  i' c@(CaseAlternative ca) = case ca.guard of
    Nothing -> foldl (<>) (i c) (map h' ca.binders) <> g' ca.result
    Just grd -> foldl (<>) (i c) (map h' ca.binders) <> g' grd <> g' ca.result

  j' e@(DoNotationValue v) = j e <> g' v
  j' e@(DoNotationBind b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet ds) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ e1) = j e <> j' e1

everythingWithContextOnValues :: forall s r.
  s ->
  r ->
  (r -> r -> r) ->
  (s -> Declaration       -> (Tuple s r)) ->
  (s -> Value             -> (Tuple s r)) ->
  (s -> Binder            -> (Tuple s r)) ->
  (s -> CaseAlternative   -> (Tuple s r)) ->
  (s -> DoNotationElement -> (Tuple s r)) ->
  { decls :: Declaration -> r
  , values :: Value -> r
  , binders :: Binder -> r
  , cases :: CaseAlternative -> r
  , dos :: DoNotationElement -> r }
everythingWithContextOnValues s0 r0 (<>) f g h i j =
  { decls: f'' s0
  , values: g'' s0
  , binders: h'' s0
  , cases: i'' s0
  , dos: j'' s0 }
  where
  f'' s d = case f s d of Tuple s' r -> r <> f' s' d

  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs Nothing val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) val
  f' s (ValueDeclaration _ _ bs (Just grd) val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) grd <> (g'' s) val
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (map (\(Tuple3 _ _ val) -> (g'' s) val) ds)
  f' s (TypeClassDeclaration _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ d1) = (f'' s) d1
  f' _ _ = r0

  g'' s v = case g s v of Tuple s' r -> r <> g' s' v

  g' s (UnaryMinus v1) = (g'' s) v1
  g' s (BinaryNoParens _ v1 v2) = (g'' s) v1 <> (g'' s) v2
  g' s (Parens v1) = (g'' s) v1
  g' s (ArrayLiteral vs) = foldl (<>) r0 (map (g'' s) vs)
  g' s (ObjectLiteral vs) = foldl (<>) r0 (map (g'' s <<< snd) vs)
  g' s (Accessor _ v1) = (g'' s) v1
  g' s (ObjectUpdate obj vs) = foldl (<>) ((g'' s) obj) (map (g'' s <<< snd) vs)
  g' s (Abs _ v1) = (g'' s) v1
  g' s (App v1 v2) = (g'' s) v1 <> (g'' s) v2
  g' s (IfThenElse v1 v2 v3) = (g'' s) v1 <> (g'' s) v2 <> (g'' s) v3
  g' s (Case vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _) = (g'' s) v1
  g' s (Let ds v1) = (foldl (<>) r0 (map (f'' s) ds)) <> (g'' s) v1
  g' s (Do es) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ v1) = (g'' s) v1
  g' _ _ = r0

  h'' s b = case h s b of Tuple s' r -> r <> h' s' b

  h' s (ConstructorBinder _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ObjectBinder bs) = foldl (<>) r0 (map (h'' s <<< snd) bs)
  h' s (ArrayBinder bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ConsBinder b1 b2) = (h'' s) b1 <> (h'' s) b2
  h' s (NamedBinder _ b1) = (h'' s) b1
  h' s (PositionedBinder _ b1) = (h'' s) b1
  h' _ _ = r0

  i'' s ca = case i s ca of Tuple s' r -> r <> i' s' ca

  i' s (CaseAlternative ca) = case ca.guard of
    Nothing -> foldl (<>) r0 (map (h'' s) ca.binders) <> (g'' s) ca.result
    Just grd -> foldl (<>) r0 (map (h'' s) ca.binders) <> (g'' s) grd <> (g'' s) ca.result

  j'' s e = case j s e of Tuple s' r -> r <> j' s' e

  j' s (DoNotationValue v) = (g'' s) v
  j' s (DoNotationBind b v) =  (h'' s) b <> (g'' s) v
  j' s (DoNotationLet ds) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ e1) = (j'' s) e1


everywhereWithContextOnValuesM :: forall s m. (Monad m) =>
  s ->
  (s -> Declaration       -> m (Tuple s Declaration)) ->
  (s -> Value             -> m (Tuple s Value)) ->
  (s -> Binder            -> m (Tuple s Binder)) ->
  (s -> CaseAlternative   -> m (Tuple s CaseAlternative)) ->
  (s -> DoNotationElement -> m (Tuple s DoNotationElement)) ->
  { decls :: Declaration -> m Declaration
  , values :: Value -> m Value
  , binders :: Binder -> m Binder
  , cases :: CaseAlternative -> m CaseAlternative
  , dos :: DoNotationElement -> m DoNotationElement }
everywhereWithContextOnValuesM s0 f g h i j =
  { decls: f'' s0
  , values: g'' s0
  , binders: h'' s0
  , cases: i'' s0
  , dos: j'' s0 }
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f'' s) ds
  f' s (ValueDeclaration name nameKind bs grd val) = ValueDeclaration name nameKind <$> traverse (h'' s) bs <*> maybeM (g'' s) grd <*> g'' s val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverse (f'' s) ds
  f' s (PositionedDeclaration pos d1) = PositionedDeclaration pos <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
  g' s (BinaryNoParens op v1 v2) = BinaryNoParens op <$> g'' s v1 <*> g'' s v2
  g' s (Parens v) = Parens <$> g'' s v
  g' s (ArrayLiteral vs) = ArrayLiteral <$> traverse (g'' s) vs
  g' s (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g'' s)) vs
  g' s (Accessor prop v) = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> traverse (sndM (g'' s)) vs
  g' s (Abs name v) = Abs name <$> g'' s v
  g' s (App v1 v2) = App <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts) = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let ds v) = Let <$> traverse (f'' s) ds <*> g'' s v
  g' s (Do es) = Do <$> traverse (j'' s) es
  g' s (PositionedValue pos v) = PositionedValue pos <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h'' s) bs
  h' s (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h'' s)) bs
  h' s (ArrayBinder bs) = ArrayBinder <$> traverse (h'' s) bs
  h' s (ConsBinder b1 b2) = ConsBinder <$> h'' s b1 <*> h'' s b2
  h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
  h' s (PositionedBinder pos b) = PositionedBinder pos <$> h'' s b
  h' _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative ca) =
    mkCaseAlternative <$> traverse (h'' s) ca.binders
                      <*> maybeM (g'' s) ca.guard
                      <*> g'' s ca.result

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue v) = DoNotationValue <$> g'' s v
  j' s (DoNotationBind b v) = DoNotationBind <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ds) = DoNotationLet <$> traverse (f'' s) ds
  j' s (PositionedDoNotationElement pos e1) = PositionedDoNotationElement pos <$> j'' s e1

accumTypes :: forall r. (Monoid r) => (Type -> r) ->
                                      { decls :: Declaration -> r
                                      , values :: Value -> r
                                      , binders :: Binder -> r
                                      , cases :: CaseAlternative -> r
                                      , dos :: DoNotationElement -> r }
accumTypes f = everythingOnValues (<>) forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ dctors) = mconcat (concatMap (map f <<< snd) dctors)
  forDecls (ExternDeclaration _ _ _ ty) = f ty
  forDecls (ExternInstanceDeclaration _ cs _ tys) = mconcat (concatMap (map f <<< snd) cs) <> mconcat (map f tys)
  forDecls (TypeClassDeclaration _ _ implies _) = mconcat (concatMap (map f <<< snd) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f <<< snd) cs) <> mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty) = f ty
  forDecls (TypeDeclaration _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary _ (Tuple _ cs) _) = mconcat (map f cs)
  forValues (SuperClassDictionary _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ ty) = f ty
  forValues _ = mempty
