module Language.PureScript.TypeChecker.Types (
    typesOf
  , DictionaryValue()
  ) where

{-
  The following functions represent the corresponding type checking judgements:

    infer
      Synthesize a type for a value

    check
      Check a value has a given type

    checkProperties
      Check an object with a given type contains specified properties

    checkFunctionApplication
      Check a function of a given type returns a value of another type when applied to its arguments

    subsumes
      Check a type subsumes another type
-}

import Data.Array ((\\), concat, delete, length, groupBy, filter, map, mapMaybe, range, nub, sort, sortBy, zipWith)
import Data.Either
import Data.Foldable (all, for_, foldl, foldr, notElem, lookup, find)
import Data.Traversable (for, traverse, zipWithA)
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import Data.Monoid.First
import Data.Tuple
import Data.Tuple3
import Data.String (joinWith)

import qualified Data.Maybe.Unsafe as Unsafe
import qualified Data.Array.Unsafe as Unsafe

import Language.PureScript.Declarations
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Pretty.Types
import Language.PureScript.Pretty.Values
import Language.PureScript.Environment
import Language.PureScript.Errors
import qualified Language.PureScript.Constants as C

import Control.Apply
import Control.Bind
import Control.Monad (replicateM)
import Control.Monad.Error
import Control.Monad.Error.Proxy

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception

import Control.Arrow (first)

import qualified Data.Map as M

instance partialType :: Partial Type where
  unknown = TUnknown
  isUnknown (TUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns = everythingOnTypes (++) go
    where
    go (TUnknown u) = [u]
    go _ = []
  ($?) sub = everywhereOnTypes go
    where
    go t@(TUnknown u) = fromMaybe t $ M.lookup u (runSubstitution sub)
    go other = other

instance unifiableCheckType :: Unifiable Type where
  unify = unifyTypes

-- |
-- Unify two types, updating the current substitution
--
unifyTypes :: RefVal (UnifyState Type) -> Type -> Type -> Check Unit
unifyTypes stRef t1 t2 = rethrowException (\x -> mkErrorStack ("Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2) Nothing <> x) $
  unifyTypes' t1 t2
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return unit
  unifyTypes' (TUnknown u) t = substitute stRef u t
  unifyTypes' t (TUnknown u) = substitute stRef u t
  unifyTypes' (SaturatedTypeSynonym name args) ty = do
    ty1 <- introduceSkolemScope stRef <=< expandTypeSynonym chSt name $ args
    ty1 `unifyTypes` ty
  unifyTypes' ty s@(SaturatedTypeSynonym _ _) = unifyTypes stRef s ty
  unifyTypes' (ForAll ident1 ty1 sc1) (ForAll ident2 ty2 sc2) =
    case Tuple sc1 sc2 of
      Tuple (Just sc1') (Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ident1 sko sc1' ty1
        let sk2 = skolemize ident2 sko sc2' ty2
        unifyTypes stRef sk1 sk2
      _ -> theImpossibleHappened "Skolemized type variable was not given a scope"
  unifyTypes' (ForAll ident ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ident sko sc ty1
    sk `unifyTypes` ty2
  unifyTypes' (ForAll _ _ _) _ = throwException $ withErrorType $ strMsg $ "Skolem variable scope is unspecified"
  unifyTypes' ty f@(ForAll _ _ _) = unifyTypes stRef f ty
  unifyTypes' (TypeVar v1) (TypeVar v2) | v1 == v2 = return unit
  unifyTypes' (TypeConstructor c1) (TypeConstructor c2) =
    guardWith (withErrorType $ strMsg ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".")) (c1 == c2)
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    unifyTypes stRef t3 t5
    unifyTypes stRef t4 t6
  unifyTypes' (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = return unit
  unifyTypes' r1@(RCons _ _ _) r2 = unifyRows stRef r1 r2
  unifyTypes' r1 r2@(RCons _ _ _) = unifyRows stRef r1 r2
  unifyTypes' REmpty r2 = unifyRows stRef REmpty r2
  unifyTypes' r1 REmpty = unifyRows stRef r1 REmpty
  unifyTypes' t@(ConstrainedType _ _) _ = throwException $ strMsg $ "Attempted to unify a constrained type " ++ prettyPrintType t ++ " with another type."
  unifyTypes' t3 t4@(ConstrainedType _ _) = unifyTypes' t4 t3
  unifyTypes' t3 t4 = throwException $ strMsg $ "Cannot unify " ++ prettyPrintType t3 ++ " with " ++ prettyPrintType t4 ++ "."

-- |
-- Unify two rows, updating the current substitution
--
-- Common labels are first identified, and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate, otherwise leftover labels result in a unification
-- error.
--
unifyRows :: RefVal (UnifyState Type) -> Type -> Type -> Check Unit
unifyRows stRef r1 r2 =
  case Tuple (rowToList r1) (rowToList r2) of
    Tuple (Tuple s1 r1') (Tuple s2 r2') ->
      let int = flip mapMaybe s1 $ \(Tuple name t1) -> (Tuple t1 <<< snd) <$> (flip find s2 $ \(Tuple name' t2) -> name == name')
          sd1 = flip filter s1 $ \(Tuple name _) -> name `notElem` map fst s2
          sd2 = flip filter s2 $ \(Tuple name _) -> name `notElem` map fst s1
      in do
        for_ int (uncurry (unify stRef))
        unifyRows' sd1 r1' sd2 r2'
  where
  unifyRows' :: [Tuple String Type] -> Type -> [Tuple String Type] -> Type -> Check Unit
  unifyRows' [] (TUnknown u) sd r = substitute stRef u (rowFromList $ Tuple sd r)
  unifyRows' sd r [] (TUnknown u) = substitute stRef u (rowFromList $ Tuple sd r)
  unifyRows' ((Tuple name ty):row) r others u@(TUnknown un) = do
    occursCheck stRef un ty
    for_ row $ \(Tuple _ t) -> occursCheck stRef un t
    u' <- fresh stRef
    unify stRef u $ RCons name ty u'
    unifyRows' row r others u'
  unifyRows' [] REmpty [] REmpty = return unit
  unifyRows' [] (TypeVar v1) [] (TypeVar v2) | v1 == v2 = return unit
  unifyRows' [] (Skolem _ s1 _) [] (Skolem _ s2 _) | s1 == s2 = return unit
  unifyRows' sd3 r3 sd4 r4 = throwException $ strMsg $
    "Cannot unify (" ++ prettyPrintRow (rowFromList $ Tuple sd3 r3) ++
    ") with (" ++ prettyPrintRow (rowFromList $ Tuple sd4 r4) ++ ")"

-- |
-- Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
--
typesOf :: Maybe ModuleName -> ModuleName -> [Tuple Ident Value] -> Check [Tuple Ident (Tuple Value Type)]
typesOf mainModuleName moduleName vals = do
  tys <- tidyUp <$> withSubstitution (\stRef -> do
    Tuple3 es dict untypedDict <- typeDictionaryForBindingGroup stRef moduleName vals
    for es $ \e -> do
      triple@(Tuple _ (Tuple _ ty)) <- typeForBindingGroupElement moduleName e dict untypedDict
      -- If --main is enabled, need to check that `main` has type Eff eff a for some eff, a
      if (Just moduleName == mainModuleName && fst e == Ident C.main)
        then do
          [eff, a] <- replicateM 2 fresh
          unify ty $ TypeApp (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Control", ProperName "Monad", ProperName "Eff"])) (ProperName "Eff"))) eff) a
          return triple
        else return triple)

  for tys $ \(Tuple ident (Tuple val ty)) -> do
    -- Replace type class dictionary placeholders with actual dictionaries
    val' <- replaceTypeClassDictionaries moduleName val
    -- Check skolem variables did not escape their scope
    skolemEscapeCheck val'
    -- Remove type synonyms placeholders, remove duplicate row fields, and replace
    -- top-level unification variables with named type variables.
    let val'' = overTypes (desaturateAllTypeSynonyms <<< setifyAll) val'
        ty' = varIfUnknown <<< desaturateAllTypeSynonyms <<< setifyAll $ ty
    return (Tuple ident (Tuple val'' ty'))
  where
  -- Apply the substitution that was returned from withSubstitution to both types and (type-annotated) values
  tidyUp (Tuple ts sub) = map (\(Tuple i (Tuple val ty)) -> (Tuple i (Tuple (overTypes (($?) sub) val) (sub $? ty)))) ts

typeDictionaryForBindingGroup :: RefVal (UnifyState Type)
                              -> ModuleName
                              -> [Tuple Ident Value]
                              -> Check (Tuple3 [Tuple Ident (Tuple Value (Maybe (Tuple Type Boolean)))]
                                               (M.Map (Tuple ModuleName Ident) (Tuple Type NameKind))
                                               [Tuple Ident Type])
typeDictionaryForBindingGroup stRef moduleName vals = do
  let
    -- Map each declaration to a name/value pair, with an optional type, if the declaration is typed
    es = map isTyped vals
    -- Filter the typed and untyped declarations
    typed = filter (isJust <<< snd <<< snd) es
    untyped = filter (isNothing <<< snd <<< snd) es
    -- Make a map of names to typed declarations
    typedDict = map (\(Tuple ident (Tuple _ (Just (Tuple ty _)))) -> Tuple ident ty) typed

  -- Create fresh unification variables for the types of untyped declarations
  untypedNames <- replicateM (length untyped) (fresh stRef)

  let
    -- Make a map of names to the unification variables of untyped declarations
    untypedDict = zip (map fst untyped) untypedNames
    -- Create the dictionary of all name/type pairs, which will be added to the environment during type checking
    dict = M.fromList (map (\(Tuple ident ty) -> (Tuple (Tuple moduleName ident) (Tuple ty LocalVariable))) $ typedDict ++ untypedDict)
  return $ Tuple3 es dict untypedDict

typeForBindingGroupElement :: ModuleName
                           -> (Tuple Ident (Tuple Value (Maybe (Tuple Type Boolean))))
                           -> M.Map (Tuple ModuleName Ident) (Tuple Type NameKind)
                           -> [Tuple Ident Type]
                           -> Check (Tuple Ident (Tuple Value Type))
typeForBindingGroupElement moduleName (Tuple ident (Tuple val t)) dict untypedDict =
  -- If the declaration is a function, it has access to other values in the binding group.
  -- If not, the generated code might fail at runtime since those values might be undefined.
  let dict' = if isFunction val then dict else M.empty :: M.Map (Tuple ModuleName Ident) (Tuple Type NameKind)
  in case t of
    -- Typed declarations
    Just (Tuple ty checkType) -> do
      -- Kind check
      kind <- kindOf moduleName ty
      guardWith (withErrorType $ strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
      -- Check the type with the new names in scope
      ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
      val' <- bindNames dict' $ if checkType
                                then TypedValue true <$> check val ty' <*> pure ty'
                                else return (TypedValue false val ty')
      return $ Tuple ident (Tuple val' ty')
    -- Untyped declarations
    Nothing -> do
      -- Infer the type with the new names in scope
      TypedValue _ val' ty <- bindNames dict' $ infer val
      unify ty $ Unsafe.fromJust (lookup ident untypedDict)
      return $ Tuple ident (Tuple (TypedValue true val' ty) ty)

-- |
-- Check if a value introduces a function
--
isFunction :: Value -> Boolean
isFunction (Abs _ _) = true
isFunction (TypedValue _ val _) = isFunction val
isFunction (PositionedValue _ val) = isFunction val
isFunction _ = false

-- |
-- Check if a value contains a type annotation
--
isTyped :: Tuple Ident Value -> Tuple Ident (Tuple Value (Maybe (Tuple Type Boolean)))
isTyped (Tuple name (TypedValue checkType value ty)) = Tuple name (Tuple value (Just $ Tuple ty checkType))
isTyped (Tuple name value) = Tuple name (Tuple value Nothing)

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: (Type -> Type) -> Value -> Value
overTypes f = (everywhereOnValues id g id).values
  where
  g :: Value -> Value
  g (TypedValue checkTy val t) = TypedValue checkTy val (f t)
  g (TypeClassDictionary b (Tuple nm tys) sco) = TypeClassDictionary b (Tuple nm (map f tys)) sco
  g other = other

-- |
-- Replace type class dictionary placeholders with inferred type class dictionaries
--
replaceTypeClassDictionaries :: ModuleName -> Value -> Check Value
replaceTypeClassDictionaries mn = (everywhereOnValuesTopDownM return go return).values
  where
  go (TypeClassDictionary trySuperclasses constraint dicts) = do
    env <- getEnv
    entails env mn dicts constraint trySuperclasses
  go other = return other

-- |
-- A simplified representation of expressions which are used to represent type
-- class dictionaries at runtime, which can be compared for equality
--
data DictionaryValue
  -- |
  -- A dictionary which is brought into scope by a local constraint
  --
  = LocalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which is brought into scope by an instance declaration
  --
  | GlobalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which depends on other dictionaries
  --
  | DependentDictionaryValue (Qualified Ident) [DictionaryValue]
  -- |
  -- A subclass dictionary
  --
  | SubclassDictionaryValue DictionaryValue (Qualified ProperName) Number

instance eqDictionaryValue :: Eq DictionaryValue where
  (==) (LocalDictionaryValue q1) (LocalDictionaryValue q2) = q1 == q2
  (==) (GlobalDictionaryValue q1) (GlobalDictionaryValue q2) = q1 == q2
  (==) (DependentDictionaryValue q1 xs) (DependentDictionaryValue q2 ys) = q1 == q2 && xs == ys
  (==) (SubclassDictionaryValue d1 q1 n1) (SubclassDictionaryValue d2 q2 n2) = d1 == d2 && q1 == q2 && n1 == n2
  (==) _ _ = false
  (/=) x y = not (x == y)

instance ordDictionaryValue :: Ord DictionaryValue where
  compare (LocalDictionaryValue q1) (LocalDictionaryValue q2) = compare q1 q2
  compare (LocalDictionaryValue _) _ = LT
  compare (GlobalDictionaryValue q1) (GlobalDictionaryValue q2) = compare q1 q2
  compare (GlobalDictionaryValue _) (LocalDictionaryValue _) = GT
  compare (GlobalDictionaryValue _) _ = LT
  compare (DependentDictionaryValue q1 xs) (DependentDictionaryValue q2 ys) = case compare q1 q2 of
    EQ -> compare xs ys
    other -> other
  compare (DependentDictionaryValue _ _) (LocalDictionaryValue _) = GT
  compare (DependentDictionaryValue _ _) (GlobalDictionaryValue _) = GT
  compare (DependentDictionaryValue _ _) _ = LT
  compare (SubclassDictionaryValue d1 q1 n1) (SubclassDictionaryValue d2 q2 n2) = case compare d1 d2 of
    EQ -> case compare q1 q2 of
      EQ -> compare n1 n2
      other' -> other'
    other -> other
  compare (SubclassDictionaryValue _ _ _) _ = GT

instance showDictionaryValue :: Show DictionaryValue where
  show (LocalDictionaryValue q) = "LocalDictionaryValue (" ++ show q ++ ")"
  show (GlobalDictionaryValue q) = "GlobalDictionaryValue (" ++ show q ++ ")"
  show (DependentDictionaryValue q xs) = "DependentDictionaryValue (" ++ show q ++ ") (" ++ show xs ++ ")"
  show (SubclassDictionaryValue d q n) = "SubclassDictionaryValue (" ++ show d ++ ") (" ++ show q ++ ") (" ++ show n ++ ")"

-- |
-- Check that the current set of type class dictionaries entail the specified type class goal, and, if so,
-- return a type class dictionary reference.
--
entails :: Environment
        -> ModuleName
        -> [TypeClassDictionaryInScope]
        -> Tuple (Qualified ProperName) [Type]
        -> Boolean
        -> Check Value
entails env@(Environment envo) moduleName context = solve (sortedNubBy canonicalizeDictionary (filter filterModule context))
  where
  sortedNubBy :: forall k v. (Ord k) => (v -> k) -> [v] -> [v]
  sortedNubBy f vs = M.values (M.fromList (map (\v -> Tuple (f v) v) vs))

  -- Filter out type dictionaries which are in scope in the current module
  filterModule :: TypeClassDictionaryInScope -> Boolean
  filterModule (TypeClassDictionaryInScope { name = Qualified (Just mn) _ }) | mn == moduleName = true
  filterModule (TypeClassDictionaryInScope { name = Qualified Nothing _ }) = true
  filterModule _ = false

  go :: [TypeClassDictionaryInScope] -> Boolean -> Qualified ProperName -> [Type] -> [DictionaryValue]
  go context' trySuperclasses' className' tys' = regularInstances ++ superclassInstances
    where
    -- Look for regular type instances
    regularInstances = do
      tcd@(TypeClassDictionaryInScope tcdo) <- context'
      -- Make sure the type class name matches the one we are trying to satisfy
      guardArray $ className' == tcdo.className
      -- Make sure the type unifies with the type in the type instance definition
      subst <- maybeToList $ do
        substAll <- concat <$> zipWithA (typeHeadsAreEqual moduleName env) tys' tcdo.instanceTypes
        verifySubstitution substAll
      -- Solve any necessary subgoals
      args <- solveSubgoals context' subst tcdo.dependencies
      return (mkDictionary (canonicalizeDictionary tcd) args)

    -- Look for implementations via superclasses
    superclassInstances | trySuperclasses' = do
      (Tuple subclassName (Tuple3 args _ implies)) <- M.toList envo.typeClasses
      -- Try each superclass
      (Tuple index (Tuple superclass suTyArgs)) <- zip (range 0 (length implies - 1)) implies
      -- Make sure the type class name matches the superclass name
      guardArray $ className' == superclass
      -- Make sure the types unify with the types in the superclass implication
      subst <- maybeToList $ do
        substAll <- concat <$> zipWithA (typeHeadsAreEqual moduleName env) tys' suTyArgs
        verifySubstitution substAll
      -- Finally, satisfy the subclass constraint
      args' <- maybeToList $ traverse (flip lookup subst) args
      suDict <- go context' true subclassName args'
      return (SubclassDictionaryValue suDict superclass index)
    superclassInstances = []

    -- Ensure that a substitution is valid
    verifySubstitution :: [Tuple String Type] -> Maybe [Tuple String Type]
    verifySubstitution subst = do
      let grps = groupBy ((==) `on` fst) subst
      if all (pairwise (unifiesWith env) <<< map snd) grps
        then Just (map Unsafe.head grps)
        else Nothing

  solveSubgoals :: [TypeClassDictionaryInScope] -> [Tuple String Type] -> Maybe [Tuple (Qualified ProperName) [Type]] -> [Maybe [DictionaryValue]]
  solveSubgoals _ _ Nothing = return Nothing
  solveSubgoals context' subst (Just subgoals) = do
    dict <- traverse (\(Tuple className ts) -> go context' true className (map (replaceAllTypeVars subst) ts)) subgoals
    return $ Just dict

  solve :: [TypeClassDictionaryInScope] -> Tuple (Qualified ProperName) [Type] -> Boolean -> Check Value
  solve context' (Tuple className tys) trySuperclasses =
    case sortedNubBy dictTrace (chooseSimplestDictionaries (go context' trySuperclasses className tys)) of
      [] -> throwException $ withErrorType $ strMsg $ "No instance found for " ++ show className ++ " " ++ joinWith " " (map prettyPrintTypeAtom tys)
      [dict] -> return $ dictionaryValueToValue dict
      _ -> throwException $ withErrorType $ strMsg $ "Overlapping instances found for " ++ show className ++ " " ++ joinWith " " (map prettyPrintTypeAtom tys)

guardArray :: Boolean -> [Unit]
guardArray true = [unit]
guardArray false = []

maybeToList :: forall a. Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Make a dictionary from subgoal dictionaries by applying the correct function
mkDictionary :: Qualified Ident -> Maybe [DictionaryValue] -> DictionaryValue
mkDictionary fnName Nothing = LocalDictionaryValue fnName
mkDictionary fnName (Just []) = GlobalDictionaryValue fnName
mkDictionary fnName (Just dicts) = DependentDictionaryValue fnName dicts

-- Turn a DictionaryValue into a Value
dictionaryValueToValue :: DictionaryValue -> Value
dictionaryValueToValue (LocalDictionaryValue fnName) = Var fnName
dictionaryValueToValue (GlobalDictionaryValue fnName) = App (Var fnName) (ObjectLiteral [])
dictionaryValueToValue (DependentDictionaryValue fnName dicts) = foldl App (Var fnName) (map dictionaryValueToValue dicts)
dictionaryValueToValue (SubclassDictionaryValue dict superclassName index) =
  App (Accessor (show superclassName ++ "_" ++ show index)
                (Accessor C.__superclasses (dictionaryValueToValue dict)))
      (ObjectLiteral [])

-- Choose the simplest DictionaryValues from a list of candidates
-- The reason for this function is as follows:
-- When considering overlapping instances, we don't want to consider the same dictionary
-- to be an overlap of itself when obtained as a superclass of another class.
-- Observing that we probably don't want to select a superclass instance when an instance
-- is available directly, and that there is no way for a superclass instance to actually
-- introduce an overlap that wouldn't have been there already, we simply remove dictionaries
-- obtained as superclass instances if there are simpler instances available.
chooseSimplestDictionaries :: [DictionaryValue] -> [DictionaryValue]
chooseSimplestDictionaries ds = case filter isSimpleDictionaryValue ds of
                                  [] -> ds
                                  simple -> simple
isSimpleDictionaryValue (SubclassDictionaryValue _ _ _) = false
isSimpleDictionaryValue (DependentDictionaryValue _ ds) = all isSimpleDictionaryValue ds
isSimpleDictionaryValue _ = true

-- |
-- Get the "trace" of a DictionaryValue - that is, remove all SubclassDictionaryValue
-- data constructors
--
dictTrace :: DictionaryValue -> DictionaryValue
dictTrace (DependentDictionaryValue fnName dicts) = DependentDictionaryValue fnName $ map dictTrace dicts
dictTrace (SubclassDictionaryValue dict _ _) = dictTrace dict
dictTrace other = other

-- |
-- Check all values in a list pairwise match a predicate
--
pairwise :: forall a. (a -> a -> Boolean) -> [a] -> Boolean
pairwise _ [] = true
pairwise _ [_] = true
pairwise p (x : xs) = all (p x) xs && pairwise p xs

-- |
-- Check that two types unify
--
unifiesWith :: Environment -> Type -> Type -> Boolean
unifiesWith _ (TUnknown u1) (TUnknown u2) | u1 == u2 = true
unifiesWith _ (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = true
unifiesWith _ (TypeVar v1) (TypeVar v2) | v1 == v2 = true
unifiesWith _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = true
unifiesWith e (TypeApp h1 t1) (TypeApp h2 t2) = unifiesWith e h1 h2 && unifiesWith e t1 t2
unifiesWith e (SaturatedTypeSynonym name args) t2 =
  case expandTypeSynonym' e name args of
    Left  _  -> false
    Right t1 -> unifiesWith e t1 t2
unifiesWith e t1 t2@(SaturatedTypeSynonym _ _) = unifiesWith e t2 t1
unifiesWith _ _ _ = false

-- |
-- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
-- and return a substitution from type variables to types which makes the type heads unify.
--
typeHeadsAreEqual :: ModuleName -> Environment -> Type -> Type -> Maybe [Tuple String Type]
typeHeadsAreEqual _ _ (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = Just []
typeHeadsAreEqual _ _ t (TypeVar v) = Just [Tuple v t]
typeHeadsAreEqual _ _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = Just []
typeHeadsAreEqual m e (TypeApp h1 t1) (TypeApp h2 t2) = (++) <$> typeHeadsAreEqual m e h1 h2 <*> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual m e (SaturatedTypeSynonym name args) t2 = case expandTypeSynonym' e name args of
  Left  _  -> Nothing
  Right t1 -> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual _ _ _ _ = Nothing

-- |
-- Ensure skolem variables do not escape their scope
--
skolemEscapeCheck :: Value -> Check Unit
skolemEscapeCheck (TypedValue false _ _) = return unit
skolemEscapeCheck root@(TypedValue _ _ _) =
  -- Every skolem variable is created when a ForAll type is skolemized.
  -- This determines the scope of that skolem variable, which is copied from the SkolemScope
  -- field of the ForAll constructor.
  -- We traverse the tree top-down, and collect any SkolemScopes introduced by ForAlls.
  -- If a Skolem is encountered whose SkolemScope is not in the current list, we have found
  -- an escaped skolem variable.
  case (everythingWithContextOnValues [] [] (++) def go def def def).values root of
   [] -> return unit
   ((Tuple binding val) : _) -> throwException $ mkErrorStack ("Rigid/skolem type variable " ++ maybe "" (\x -> "bound by " ++ prettyPrintValue x) binding ++ " has escaped.") (Just (ValueError val))
  where
  def :: forall a. [Tuple SkolemScope Value] -> a -> Tuple [Tuple SkolemScope Value] [Tuple (Maybe Value) Value]
  def s _ = Tuple s []

  go :: [Tuple SkolemScope Value] -> Value -> (Tuple [Tuple SkolemScope Value] [Tuple (Maybe Value) Value])
  go scos val@(TypedValue _ _ (ForAll _ _ (Just sco))) = Tuple ((Tuple sco val) : scos) []
  go scos val@(TypedValue _ _ ty) = case collectSkolems ty \\ map fst scos of
                                      (sco : _) -> Tuple scos [Tuple (findBindingScope sco) val]
                                      _ -> Tuple scos []
    where
    collectSkolems :: Type -> [SkolemScope]
    collectSkolems = nub <<< everythingOnTypes (++) collect
      where
      collect (Skolem _ _ scope) = [scope]
      collect _ = []
  go scos _ = Tuple scos []

  findBindingScope :: SkolemScope -> Maybe Value
  findBindingScope sco = runFirst $ (everythingOnValues (<>) (const mempty) go' (const mempty) (const mempty) (const mempty)).values root
    where
    go' val@(TypedValue _ _ (ForAll _ _ (Just sco'))) | sco == sco' = First (Just val)
    go' _ = mempty
skolemEscapeCheck val = throwException $ mkErrorStack "Untyped value passed to skolemEscapeCheck" (Just (ValueError val))

-- |
-- Ensure a row contains no duplicate labels
--
setify :: Type -> Type
setify = rowFromList <<< first (M.toList <<< M.fromList) <<< rowToList

-- |
-- \"Setify\" all rows occuring inside a value
--
setifyAll :: Type -> Type
setifyAll = everywhereOnTypes setify

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: Type -> Type
varIfUnknown ty =
  let unks = nub $ unknowns ty
      toName x = "t" ++ show x
      ty' = everywhereOnTypes typeToVar ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown u) = TypeVar (toName u)
      typeToVar t = t
  in mkForAll (sort <<< map toName $ unks) ty'

-- |
-- Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
--
instantiatePolyTypeWithUnknowns :: RefVal CheckState -> RefVal (UnifyState Type) -> Value -> Type -> Check (Tuple Value Type)
instantiatePolyTypeWithUnknowns chSt stRef val (ForAll ident ty _) = do
  ty' <- replaceVarWithUnknown stRef ident ty
  instantiatePolyTypeWithUnknowns chSt stRef val ty'
instantiatePolyTypeWithUnknowns chSt stRef val (ConstrainedType constraints ty) = do
   dicts <- getTypeClassDictionaries chSt
   Tuple _ ty' <- instantiatePolyTypeWithUnknowns chSt stRef (error "Types under a constraint cannot themselves be constrained") ty
   return $ Tuple (foldl App val (map (flip (TypeClassDictionary true) dicts) constraints)) ty'
instantiatePolyTypeWithUnknowns _ _ val ty = return $ Tuple val ty

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: RefVal (UnifyState Type) -> String -> Type -> Check Type
replaceVarWithUnknown stRef ident ty = do
  tu <- fresh stRef
  return $ replaceTypeVars ident tu ty

-- |
-- Replace fully applied type synonyms with the @SaturatedTypeSynonym@ data constructor, which helps generate
-- better error messages during unification.
--
replaceAllTypeSynonyms' :: Environment -> Type -> Either String Type
replaceAllTypeSynonyms' (Environment e) d =
  let syns = map (\(Tuple name (Tuple args _)) -> (Tuple name (length args))) $ M.toList e.typeSynonyms
  in saturateAllTypeSynonyms syns d

replaceAllTypeSynonyms :: RefVal CheckState -> Type -> Check Type
replaceAllTypeSynonyms chSt d = do
  env <- getEnv chSt
  either (throwException <<< strMsg) return $ replaceAllTypeSynonyms' env d

-- |
-- \"Desaturate\" @SaturatedTypeSynonym@s
--
desaturateAllTypeSynonyms :: Type -> Type
desaturateAllTypeSynonyms = everywhereOnTypes replaceSaturatedTypeSynonym
  where
  replaceSaturatedTypeSynonym (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replaceSaturatedTypeSynonym t = t

-- |
-- Replace a type synonym and its arguments with the aliased type
--
expandTypeSynonym' :: Environment -> Qualified ProperName -> [Type] -> Either String Type
expandTypeSynonym' env@(Environment e) name args =
  case M.lookup name e.typeSynonyms of
    Just (Tuple synArgs body) -> do
      let repl = replaceAllTypeVars (zip synArgs args) body
      replaceAllTypeSynonyms' env repl
    Nothing -> theImpossibleHappened "Type synonym was not defined"

expandTypeSynonym :: RefVal CheckState -> Qualified ProperName -> [Type] -> Check Type
expandTypeSynonym chSt name args = do
  env <- getEnv chSt
  either (throwException <<< strMsg) return $ expandTypeSynonym' env name args

expandAllTypeSynonyms :: Type -> Check Type
expandAllTypeSynonyms = everywhereOnTypesTopDownM go
  where
  go (SaturatedTypeSynonym name args) = expandTypeSynonym name args
  go other = return other

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: [Tuple String Value] -> Check Unit
ensureNoDuplicateProperties ps =
  guardWith (withErrorType $ strMsg "Duplicate property names") $
    length (nub <<< map fst $ ps) == length ps

-- |
-- Infer a type for a value, rethrowing any error to provide a more useful error message
--
infer :: Value -> Check Value
infer val = rethrowException (\x -> mkErrorStack "Error inferring type of value" (Just (ValueError val)) <> x) $ infer' val

-- |
-- Infer a type for a value
--
infer' :: Value -> Check Value
infer' v@(NumericLiteral _) = return $ TypedValue true v tyNumber
infer' v@(StringLiteral _) = return $ TypedValue true v tyString
infer' v@(BooleanLiteral _) = return $ TypedValue true v tyBoolean
infer' (ArrayLiteral vals) = do
  ts <- traverse infer vals
  els <- fresh
  for_ ts $ \(TypedValue _ _ t) -> unify els $ TypeApp tyArray t
  return $ TypedValue true (ArrayLiteral ts) els
infer' (ObjectLiteral ps) = do
  ensureNoDuplicateProperties ps
  ts <- traverse (infer <<< snd) ps
  let fields = zipWith (\name (TypedValue _ _ t) -> Tuple name t) (map fst ps) ts
      ty = TypeApp tyObject $ rowFromList (Tuple fields REmpty)
  return $ TypedValue true (ObjectLiteral (zip (map fst ps) ts)) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- fresh
  newVals <- zipWith (\(Tuple name _) t -> Tuple name t) ps <$> traverse (infer <<< snd) ps
  let newTys = map (\(Tuple name (TypedValue _ _ ty)) -> Tuple name ty) newVals
  oldTys <- zip (map fst ps) <$> replicateM (length ps) fresh
  let oldTy = TypeApp tyObject $ rowFromList (Tuple oldTys row)
  o' <- TypedValue true <$> check o oldTy <*> pure oldTy
  return $ TypedValue true (ObjectUpdate o' newVals) $ TypeApp tyObject $ rowFromList (Tuple newTys row)
infer' (Accessor prop val) = do
  typed@(TypedValue _ _ objTy) <- infer val
  propTy <- inferProperty objTy prop
  case propTy of
    Nothing -> do
      field <- fresh
      rest <- fresh
      _ <- subsumes Nothing objTy (TypeApp tyObject (RCons prop field rest))
      return $ TypedValue true (Accessor prop typed) field
    Just ty -> return $ TypedValue true (Accessor prop typed) ty
infer' (Abs (Left arg) ret) = do
  ty <- fresh
  Just moduleName <- getCurrentModule
  bindLocalVariables moduleName [Tuple arg ty] $ do
    body@(TypedValue _ _ bodyTy) <- infer' ret
    return $ TypedValue true (Abs (Left arg) body) $ function ty bodyTy
infer' (Abs (Right _) _) = theImpossibleHappened "Binder was not desugared"
infer' (App f arg) = do
  f'@(TypedValue _ _ ft) <- infer f
  Tuple ret app <- checkFunctionApplication f' ft arg Nothing
  return $ TypedValue true app ret
infer' (Var var) = do
  Just moduleName <- getCurrentModule
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable moduleName $ var
  case ty of
    ConstrainedType constraints ty' -> do
      dicts <- getTypeClassDictionaries
      return $ TypedValue true (foldl App (Var var) (map (flip (TypeClassDictionary true) dicts) constraints)) ty'
    _ -> return $ TypedValue true (Var var) ty
infer' v@(Constructor c) = do
  Environment e <- getEnv
  case M.lookup c e.dataConstructors of
    Nothing -> throwException $ withErrorType $ strMsg $ "Constructor " ++ show c ++ " is undefined"
    Just (Tuple _ ty) -> do
      ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
      return $ TypedValue true v ty'
infer' (Case vals binders) = do
  ts <- traverse infer vals
  ret <- fresh
  binders' <- checkBinders (map (\(TypedValue _ _ t) -> t) ts) ret binders
  return $ TypedValue true (Case ts binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- check cond tyBoolean
  v2@(TypedValue _ _ t2) <- infer th
  v3@(TypedValue _ _ t3) <- infer el
  unify t2 $ t3
  return $ TypedValue true (IfThenElse cond' v2 v3) t2
infer' (Let ds val) = do
  Tuple ds' val'@(TypedValue _ _ valTy) <- inferLetBinding [] ds val infer
  return $ TypedValue true (Let ds' val') valTy
infer' (SuperClassDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary false (Tuple className tys) dicts
infer' (TypedValue checkType val ty) = do
  Just moduleName <- getCurrentModule
  kind <- kindOf moduleName ty
  guardWith (withErrorType $ strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  val' <- if checkType then check val ty' else return val
  return $ TypedValue true val' ty'
infer' (PositionedValue pos val) = rethrowExceptionWithPosition pos $ infer' val
infer' _ = theImpossibleHappened "Invalid argument to infer"

inferLetBinding :: [Declaration] -> [Declaration] -> Value -> (Value -> Check Value) -> Check (Tuple [Declaration] Value)
inferLetBinding seen [] ret j = Tuple seen <$> j ret
inferLetBinding seen (ValueDeclaration ident nameKind [] Nothing (tv@(TypedValue checkType val ty)) : rest) ret j = do
  Just moduleName <- getCurrentModule
  kind <- kindOf moduleName ty
  guardWith (withErrorType $ strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  let
    dict :: M.Map (Tuple ModuleName Ident) (Tuple Type NameKind)
    dict | isFunction val = M.singleton (Tuple moduleName ident) (Tuple ty nameKind)
    dict = M.empty
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  TypedValue _ val' ty'' <- if checkType then bindNames dict (check val ty') else return tv
  bindNames (M.singleton (Tuple moduleName ident) (Tuple ty'' nameKind)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] Nothing (TypedValue checkType val' ty'')]) rest ret j
inferLetBinding seen (ValueDeclaration ident nameKind [] Nothing val : rest) ret j = do
  valTy <- fresh
  Just moduleName <- getCurrentModule
  let
    dict :: M.Map (Tuple ModuleName Ident) (Tuple Type NameKind)
    dict | isFunction val = M.singleton (Tuple moduleName ident) (Tuple valTy nameKind)
    dict = M.empty
  TypedValue _ val' valTy' <- bindNames dict $ infer val
  unify valTy $ valTy'
  bindNames (M.singleton (Tuple moduleName ident) (Tuple valTy' nameKind)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] Nothing val']) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- getCurrentModule
  Tuple3 es dict untypedDict <- typeDictionaryForBindingGroup moduleName (map (\(Tuple3 i _ v) -> Tuple i v) ds)
  ds' <- for es $ \e -> do
    Tuple ident (Tuple val' _) <- typeForBindingGroupElement moduleName e dict untypedDict
    return $ Tuple3 ident LocalVariable val'
  bindNames dict $ inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding seen (PositionedDeclaration pos d : ds) ret j = rethrowExceptionWithPosition pos $ do
  Tuple (d' : ds') val' <- inferLetBinding seen (d : ds) ret j
  return $ Tuple (PositionedDeclaration pos d' : ds') val'
inferLetBinding _ _ _ _ = theImpossibleHappened "Invalid argument to inferLetBinding"

-- |
-- Infer the type of a property inside a record with a given type
--
inferProperty :: RefVal CheckState -> RefVal (UnifyState Type) -> Type -> String -> Check (Maybe Type)
inferProperty chSt stRef (TypeApp obj row) prop | obj == tyObject = case rowToList row of
  Tuple props _ -> return $ lookup prop props
inferProperty chSt stRef (SaturatedTypeSynonym name args) prop = do
  replaced <- introduceSkolemScope stRef <=< expandTypeSynonym chSt name $ args
  inferProperty chSt stRef replaced prop
inferProperty chSt stRef (ForAll ident ty _) prop = do
  replaced <- replaceVarWithUnknown stRef ident ty
  inferProperty chSt stRef replaced prop
inferProperty _ _ _ _ = return Nothing

-- |
-- Infer the types of variables brought into scope by a binder
--
inferBinder :: RefVal CheckState -> RefVal (UnifyState Type) -> Type -> Binder -> Check (M.Map Ident Type)
inferBinder _ _ _ NullBinder = return M.empty
inferBinder chSt stRef val (StringBinder _) = unify stRef val tyString *> return M.empty
inferBinder chSt stRef val (NumberBinder _) = unify stRef val tyNumber *> return M.empty
inferBinder chSt stRef val (BooleanBinder _) = unify stRef val tyBoolean *> return M.empty
inferBinder chSt stRef val (VarBinder name) = return $ M.singleton name val
inferBinder chSt stRef val (ConstructorBinder ctor binders) = getEnv chSt >>= \(Environment e) ->
  case M.lookup ctor e.dataConstructors of
    Nothing -> throwException $ strMsg $ "Constructor " ++ show ctor ++ " is not defined"
    Just (Tuple _ ty) -> do
      Tuple _ fn <- instantiatePolyTypeWithUnknowns chSt stRef (error "Data constructor types cannot contain constraints") ty
      fn' <- replaceAllTypeSynonyms chSt fn
      go binders fn'
        where
        go [] ty' = do
          _ <- subsumes chSt stRef Nothing val ty'
          return M.empty
        go (binder : binders') (TypeApp (TypeApp t obj) ret) | t == tyFunction =
          M.union <$> inferBinder chSt stRef obj binder <*> go binders' ret
        go _ _ = throwException $ strMsg $ "Wrong number of arguments to constructor " ++ show ctor
inferBinder chSt stRef val (ObjectBinder props) = do
  row <- fresh stRef
  rest <- fresh stRef
  m1 <- inferRowProperties row rest props
  unify stRef val $ TypeApp tyObject row
  return m1
  where
  inferRowProperties :: Type -> Type -> [Tuple String Binder] -> Check (M.Map Ident Type)
  inferRowProperties nrow row [] = (unify stRef nrow row) *> return M.empty
  inferRowProperties nrow row ((Tuple name binder):binders) = do
    propTy <- fresh stRef
    m1 <- inferBinder chSt stRef propTy binder
    m2 <- inferRowProperties nrow (RCons name propTy row) binders
    return $ m1 `M.union` m2
inferBinder chSt stRef val (ArrayBinder binders) = do
  el <- fresh stRef
  m1 <- M.unions <$> traverse (inferBinder chSt stRef el) binders
  unify stRef val $ TypeApp tyArray el
  return m1
inferBinder chSt stRef val (ConsBinder headBinder tailBinder) = do
  el <- fresh stRef
  m1 <- inferBinder chSt stRef el headBinder
  m2 <- inferBinder chSt stRef val tailBinder
  unify stRef val $ TypeApp tyArray el
  return $ m1 `M.union` m2
inferBinder chSt stRef val (NamedBinder name binder) = do
  m <- inferBinder chSt stRef val binder
  return $ M.insert name val m
inferBinder chSt stRef val (PositionedBinder pos binder) =
  rethrowExceptionWithPosition pos $ inferBinder chSt stRef val binder

-- |
-- Generate a new skolem constant
--
newSkolemConstant :: RefVal (UnifyState Type) -> Check Number
newSkolemConstant = fresh'

-- |
-- Generate a new skolem scope
--
newSkolemScope :: RefVal (UnifyState Type) -> Check SkolemScope
newSkolemScope stRef = SkolemScope <$> fresh' stRef

-- |
-- Skolemize a type variable by replacing its instances with fresh skolem constants
--
skolemize :: String -> Number -> SkolemScope -> Type -> Type
skolemize ident sko scope = replaceTypeVars ident (Skolem ident sko scope)

-- |
-- This function has one purpose - to skolemize type variables appearing in a
-- SuperClassDictionary placeholder. These type variables are somewhat unique since they are the
-- only example of scoped type variables.
--
skolemizeTypesInValue :: String -> Number -> SkolemScope -> Value -> Value
skolemizeTypesInValue ident sko scope = (everywhereOnValues id go id).values
  where
  go (SuperClassDictionary c ts) = SuperClassDictionary c (map (skolemize ident sko scope) ts)
  go other = other

-- |
-- Introduce skolem scope at every occurence of a ForAll
--
introduceSkolemScope :: RefVal (UnifyState Type) -> Type -> Check Type
introduceSkolemScope stRef = everywhereOnTypesM go
  where
  go (ForAll ident ty Nothing) = ForAll ident ty <$> (Just <$> newSkolemScope stRef)
  go other = return other

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check :: Value -> Type -> Check Value
check val ty = rethrowException (\x -> mkErrorStack errorMessage (Just (ValueError val)) <> x) $ check' val ty
  where
  errorMessage =
    "Error checking type of term " ++
    prettyPrintValue val ++
    " against type " ++
    prettyPrintType ty

-- |
-- Check the type of a value
--
check' :: Value -> Type -> Check Value
check' val (ForAll ident ty _) = do
  scope <- newSkolemScope
  sko <- newSkolemConstant
  let sk = skolemize ident sko scope ty
  let skVal = skolemizeTypesInValue ident sko scope val
  val' <- check skVal sk
  return $ TypedValue true val' (ForAll ident ty (Just scope))
check' val t@(ConstrainedType constraints ty) = do
  dictNames <- for constraints $ \(Tuple (Qualified _ (ProperName className)) _) -> do
    n <- freshDictionaryName
    return $ Ident $ "__dict_" ++ className ++ "_" ++ show n
  val' <- withTypeClassDictionaries (zipWith (\name (Tuple className instanceTy) ->
    TypeClassDictionaryInScope { name: name, className: className, instanceTypes: instanceTy, dependencies: Nothing, ty: TCDRegular }) (map (Qualified Nothing) dictNames)
      constraints) $ check val ty
  return $ TypedValue true (foldr (Abs <<< Left) val' dictNames) t
check' val (SaturatedTypeSynonym name args) = do
  ty <- introduceSkolemScope <=< expandTypeSynonym name $ args
  check val ty
check' val u@(TUnknown _) = do
  val'@(TypedValue _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (Tuple val'' ty') <- instantiatePolyTypeWithUnknowns val' ty
  unify ty' u
  return $ TypedValue true val'' ty'
check' v@(NumericLiteral _) t | t == tyNumber =
  return $ TypedValue true v t
check' v@(StringLiteral _) t | t == tyString =
  return $ TypedValue true v t
check' v@(BooleanLiteral _) t | t == tyBoolean =
  return $ TypedValue true v t
check' (ArrayLiteral vals) t@(TypeApp a ty) = do
  unify a tyArray
  array <- ArrayLiteral <$> for vals (\x -> x `check` ty)
  return $ TypedValue true array t
check' (Abs (Left arg) ret) ty@(TypeApp (TypeApp t argTy) retTy) | t == tyFunction = do
  Just moduleName <- getCurrentModule
  ret' <- bindLocalVariables moduleName [Tuple arg argTy] $ check ret retTy
  return $ TypedValue true (Abs (Left arg) ret') ty
check' (Abs (Right _) _) _ = theImpossibleHappened "Binder was not desugared"
check' (App f arg) ret = do
  f'@(TypedValue _ _ ft) <- infer f
  Tuple _ app <- checkFunctionApplication f' ft arg (Just ret)
  return $ TypedValue true app ret
check' v@(Var var) ty = do
  Just moduleName <- getCurrentModule
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable moduleName $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  v' <- subsumes (Just v) repl ty'
  case v' of
    Nothing -> throwException $ withErrorType $ strMsg $ "Unable to check type subsumption"
    Just v'' -> return $ TypedValue true v'' ty'
check' (SuperClassDictionary className tys) _ = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  --
  -- Note also that the first argument to TypeClassDictionary is false, meaning we _do not_ want
  -- to consider superclass instances when searching for this dictionary - doing so might lead
  -- to traversing a cycle in the instance graph.
  -}
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary false (Tuple className tys) dicts
check' (TypedValue checkType val ty1) ty2 = do
  Just moduleName <- getCurrentModule
  kind <- kindOf moduleName ty1
  guardWith (withErrorType $ strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
  val' <- subsumes (Just val) ty1' ty2
  case val' of
    Nothing -> throwException $ withErrorType $ strMsg $ "Unable to check type subsumption"
    Just val'' -> do
      val''' <- if checkType then check val'' ty1' else return val''
      return $ TypedValue checkType (TypedValue true val''' ty1) ty2
check' (Case vals binders) ret = do
  vals' <- traverse infer vals
  let ts = map (\(TypedValue _ _ t) -> t) vals'
  binders' <- checkBinders ts ret binders
  return $ TypedValue true (Case vals' binders') ret
check' (IfThenElse cond th el) ty = do
  cond' <- check cond tyBoolean
  th' <- check th ty
  el' <- check el ty
  return $ TypedValue true (IfThenElse cond' th' el') ty
check' (ObjectLiteral ps) t@(TypeApp obj row) | obj == tyObject = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties ps row false
  return $ TypedValue true (ObjectLiteral ps') t
check' (ObjectUpdate obj ps) t@(TypeApp o row) | o == tyObject = do
  ensureNoDuplicateProperties ps
  us <- zip (map fst ps) <$> replicateM (length ps) fresh
  case rowToList row of
    Tuple propsToCheck rest ->
      do let propsToRemove = map fst ps
         let remainingProps = filter (\(Tuple p _) -> notElem p propsToRemove) propsToCheck
         obj' <- check obj (TypeApp tyObject (rowFromList (Tuple (us ++ remainingProps) rest)))
         ps' <- checkProperties ps row true
         return (TypedValue true (ObjectUpdate obj' ps') t)
check' (Accessor prop val) ty = do
  rest <- fresh
  val' <- check val (TypeApp tyObject (RCons prop ty rest))
  return $ TypedValue true (Accessor prop val') ty
check' (Constructor c) ty = getEnv >>= \(Environment e) ->
  case M.lookup c e.dataConstructors of
    Nothing -> throwException $ withErrorType $ strMsg $ "Constructor " ++ show c ++ " is undefined"
    Just (Tuple _ ty1) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      _ <- subsumes Nothing repl ty
      return $ TypedValue true (Constructor c) ty
check' (Let ds val) ty = do
  Tuple ds' val' <- inferLetBinding [] ds val (flip check ty)
  return $ TypedValue true (Let ds' val') ty
check' val ty | containsTypeSynonyms ty = do
  ty' <- introduceSkolemScope <=< expandAllTypeSynonyms $ ty
  check val ty'
check' (PositionedValue pos val) ty =
  rethrowExceptionWithPosition pos $ check val ty
check' val ty = throwException $ mkErrorStack ("Value does not have type " ++ prettyPrintType ty) (Just (ValueError val))

containsTypeSynonyms :: Type -> Boolean
containsTypeSynonyms = everythingOnTypes (||) go where
  go (SaturatedTypeSynonym _ _) = true
  go _ = false

-- |
-- Check the types of the return values in a set of binders in a case statement
--
checkBinders :: [Type] -> Type -> [CaseAlternative] -> Check [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative { binders = binders, guard = grd, result = val } : bs) = do
  Just moduleName <- getCurrentModule
  m1 <- M.unions <$> zipWithA inferBinder nvals binders
  r <- bindLocalVariables moduleName (M.toList m1) $ do
    val' <- TypedValue true <$> check val ret <*> pure ret
    case grd of
      Nothing -> return $ mkCaseAlternative binders Nothing val'
      Just g -> do
        g' <- check g tyBoolean
        return $ mkCaseAlternative binders (Just g') val'
  rs <- checkBinders nvals ret bs
  return $ r : rs

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties :: [Tuple String Value] -> Type -> Boolean -> Check [Tuple String Value]
checkProperties ps row lax = case rowToList row of Tuple ts r' -> go ps ts r'
  where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _) = do unify u REmpty
                               return []
  go [] [] (Skolem _ _ _) | lax = return []
  go [] ((Tuple p _): _) _ =
    if lax
    then return []
    else throwException $ mkErrorStack ("Object does not have property " ++ p) (Just (ValueError (ObjectLiteral ps)))
  go ((Tuple p _):_) [] REmpty = throwException $ mkErrorStack ("Property " ++ p ++ " is not present in closed object type " ++ prettyPrintRow row) (Just (ValueError (ObjectLiteral ps)))
  go ((Tuple p v):ps') [] u@(TUnknown _) = do
    v'@(TypedValue _ _ ty) <- infer v
    rest <- fresh
    unify u $ RCons p ty rest
    ps'' <- go ps' [] rest
    return $ (Tuple p v') : ps''
  go ((Tuple p v):ps') ts r =
    case lookup p ts of
      Nothing -> do
        v'@(TypedValue _ _ ty) <- infer v
        rest <- fresh
        unify r $ RCons p ty rest
        ps'' <- go ps' ts rest
        return $ (Tuple p v') : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (Tuple p ty) ts) r
        return $ (Tuple p v') : ps''
  go _ _ _ = throwException $ mkErrorStack ("Object does not have type " ++ prettyPrintType (TypeApp tyObject row)) (Just (ValueError (ObjectLiteral ps)))

-- |
-- Check the type of a function application, rethrowing errors to provide a better error message
--
checkFunctionApplication :: Value -> Type -> Value -> Maybe Type -> Check (Tuple Type Value)
checkFunctionApplication fn fnTy arg ret = rethrowException (\x -> mkErrorStack errorMessage (Just (ValueError fn)) <> x) $ checkFunctionApplication' fn fnTy arg ret
  where
  errorMessage = "Error applying function of type "
    ++ prettyPrintType fnTy
    ++ " to argument " ++ prettyPrintValue arg

-- |
-- Check the type of a function application
--
checkFunctionApplication' :: RefVal (UnifyState Type) -> Value -> Type -> Value -> Maybe Type -> Check (Tuple Type Value)
checkFunctionApplication' stRef fn (TypeApp (TypeApp tyFunction' argTy) retTy) arg ret = do
  unify stRef tyFunction' tyFunction
  _ <- maybe (return Nothing) (subsumes Nothing retTy) ret
  subst <- (\(UnifyState st) -> st.currentSubstitution) <$> readRef stRef
  arg' <- check arg (subst $? argTy)
  return $ Tuple retTy (App fn arg')
checkFunctionApplication' stRef fn (ForAll ident ty _) arg ret = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced arg ret
checkFunctionApplication' stRef fn u@(TUnknown _) arg ret = do
  arg' <- do
    TypedValue _ arg' t <- infer arg
    Tuple arg'' t' <- instantiatePolyTypeWithUnknowns arg' t
    return $ TypedValue true arg'' t'
  let ty = (\(TypedValue _ _ t) -> t) arg'
  ret' <- maybe fresh return ret
  unify stRef u $ function ty ret'
  return $ Tuple ret' (App fn arg')
checkFunctionApplication' stRef fn (SaturatedTypeSynonym name tyArgs) arg ret = do
  ty <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  checkFunctionApplication fn ty arg ret
checkFunctionApplication' stRef fn (ConstrainedType constraints fnTy) arg ret = do
  dicts <- getTypeClassDictionaries
  checkFunctionApplication' (foldl App fn (map (flip (TypeClassDictionary true) dicts) constraints)) fnTy arg ret
checkFunctionApplication' _ _ fnTy arg _ = throwException $ withErrorType $ strMsg $ "Cannot apply a function of type "
  ++ prettyPrintType fnTy
  ++ " to argument " ++ prettyPrintValue arg


-- |
-- Check whether one type subsumes another, rethrowing errors to provide a better error message
--
subsumes :: RefVal CheckState -> RefVal (UnifyState Type) -> Maybe Value -> Type -> Type -> Check (Maybe Value)
subsumes chSt stRef val ty1 ty2 = rethrowException (\x -> mkErrorStack errorMessage (ValueError <$> val) <> x) $ subsumes' val ty1 ty2
  where
  errorMessage = "Error checking that type "
    ++ prettyPrintType ty1
    ++ " subsumes type "
    ++ prettyPrintType ty2
    
  -- |
  -- Check whether one type subsumes another
  --
  subsumes' :: Maybe Value -> Type -> Type -> Check (Maybe Value)
  subsumes' val (ForAll ident ty1 _) ty2 = do
    replaced <- replaceVarWithUnknown stRef ident ty1
    subsumes chSt stRef val replaced ty2
  subsumes' val ty1 (ForAll ident ty2 sco) =
    case sco of
      Just sco' -> do
        sko <- newSkolemConstant stRef
        let sk = skolemize ident sko sco' ty2
        subsumes chSt stRef val ty1 sk
      Nothing -> throwException $ strMsg $ "Skolem variable scope is unspecified"
  subsumes' val (TypeApp (TypeApp f1 arg1) ret1) (TypeApp (TypeApp f2 arg2) ret2) | f1 == tyFunction && f2 == tyFunction = do
    _ <- subsumes chSt stRef Nothing arg2 arg1
    _ <- subsumes chSt stRef Nothing ret1 ret2
    return val
  subsumes' val (SaturatedTypeSynonym name tyArgs) ty2 = do
    ty1 <- introduceSkolemScope stRef <=< expandTypeSynonym chSt name $ tyArgs
    subsumes chSt stRef val ty1 ty2
  subsumes' val ty1 (SaturatedTypeSynonym name tyArgs) = do
    ty2 <- introduceSkolemScope stRef <=< expandTypeSynonym chSt name $ tyArgs
    subsumes chSt stRef val ty1 ty2
  subsumes' (Just val) (ConstrainedType constraints ty1) ty2 = do
    dicts <- getTypeClassDictionaries chSt
    _ <- subsumes' Nothing ty1 ty2
    return <<< Just $ foldl App val (map (flip (TypeClassDictionary true) dicts) constraints)
  subsumes' val (TypeApp f1 r1) (TypeApp f2 r2) | f1 == tyObject && f2 == tyObject =
    case Tuple (rowToList r1) (rowToList r2) of
      Tuple (Tuple ts1 r1') (Tuple ts2 r2') ->
        let ts1' = sortBy (compare `on` fst) ts1
            ts2' = sortBy (compare `on` fst) ts2
        in do
          go ts1' ts2' r1' r2'
          return val
    where
    go [] ts2 r1' r2' = unify stRef r1' $ rowFromList (Tuple ts2 r2')
    go ts1 [] r1' r2' = unify stRef r2' $ rowFromList (Tuple ts1 r1')
    go ((Tuple p1 ty1) : ts1) ((Tuple p2 ty2) : ts2) r1' r2' = case unit of
        _ | p1 == p2 -> do
          _ <- subsumes chSt stRef Nothing ty1 ty2
          go ts1 ts2 r1' r2'
        _ | p1 < p2 -> do
          rest <- fresh stRef
          unify stRef r2' $ RCons p1 ty1 rest
          go ts1 ((Tuple p2 ty2) : ts2) r1' rest
        _ -> do
          rest <- fresh stRef
          unify stRef r1' $ RCons p2 ty2 rest
          go ((Tuple p1 ty1) : ts1) ts2 rest r2'
  subsumes' val ty1 ty2@(TypeApp obj _) | obj == tyObject = subsumes chSt stRef val ty2 ty1
  subsumes' val ty1 ty2 = do
    unify stRef ty1 $ ty2
    return val
