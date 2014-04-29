-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for types
--
-----------------------------------------------------------------------------

module Language.PureScript.Types where

import Prelude

import Data.Array (concatMap, map, nub)
import Data.Maybe
import Data.Tuple
import Data.Traversable
import Data.Foldable

import Control.Bind
import Control.Monad.Unify

import Control.Arrow (second)

import Language.PureScript.Names

-- |
-- An identifier for the scope of a skolem variable
--
data SkolemScope = SkolemScope Number

runSkolemScope :: SkolemScope -> Number
runSkolemScope (SkolemScope n) = n

instance showSkolemScope :: Show SkolemScope where
  show (SkolemScope n) = "SkolemScope " ++ show n

instance eqSkolemScope :: Eq SkolemScope where
  (==) (SkolemScope x) (SkolemScope y) = x == y
  (/=) (SkolemScope x) (SkolemScope y) = x /= y

-- |
-- The type of types
--
data Type
  -- |
  -- A unification variable of type Type
  --
  = TUnknown Unknown
  -- |
  -- A named type variable
  --
  | TypeVar String
  -- |
  -- A type constructor
  --
  | TypeConstructor (Qualified ProperName)
  -- |
  -- A type application
  --
  | TypeApp Type Type
  -- |
  -- A type synonym which is "saturated", i.e. fully applied
  --
  | SaturatedTypeSynonym (Qualified ProperName) [Type]
  -- |
  -- Forall quantifier
  --
  | ForAll String Type (Maybe SkolemScope)
  -- |
  -- A type with a set of type class constraints
  --
  | ConstrainedType [Tuple (Qualified ProperName) [Type]] Type
  -- |
  -- A skolem constant
  --
  | Skolem Number SkolemScope
  -- |
  -- An empty row
  --
  | REmpty
  -- |
  -- A non-empty row
  --
  | RCons String Type Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintFunction Type Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintArray Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintObject Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintForAll [String] Type

instance showType :: Show Type where
  show (TUnknown u) = "TUnknown " ++ show u
  show (TypeVar var) = "TypeVar " ++ show var
  show (TypeConstructor name) = "TypeConstructor " ++ show name
  show (TypeApp t1 t2) = "TypeApp " ++ show t1 ++ " " ++ show t2
  show (SaturatedTypeSynonym name ts) = "SaturatedTypeSynonym " ++ show name ++ " " ++ show ts
  show (ForAll name ty scope) = "ForAll " ++ show name ++ " " ++ show ty ++ " " ++ show scope
  show (ConstrainedType cs ty) = "ConstrainedType " ++ show cs ++ " " ++ show ty
  show (Skolem n scope) = "Skolem " ++ show n ++ " " ++ show scope
  show REmpty = "REmpty"
  show (RCons name t1 t2) = "RCons " ++ show name ++ " " ++ show t1 ++ " " ++ show t2
  show (PrettyPrintFunction t1 t2) = "PrettyPrintFunction " ++ show t1 ++ " " ++ show t2
  show (PrettyPrintArray t) = "PrettyPrintArray " ++ show t
  show (PrettyPrintObject t) = "PrettyPrintObject " ++ show t
  show (PrettyPrintForAll q t) = "PrettyPrintForAll " ++ show q ++ " " ++ show t

instance eqType :: Eq Type where
  (==) (TUnknown u1)                    (TUnknown u2)                     = u1 == u2
  (==) (TypeVar var1)                   (TypeVar var2)                    = var1 == var2
  (==) (TypeConstructor name1)          (TypeConstructor name2)           = name1 == name2
  (==) (TypeApp tx1 ty1)                (TypeApp tx2 ty2)                 = tx1 == tx2 && ty1 == ty2
  (==) (SaturatedTypeSynonym name1 ts1) (SaturatedTypeSynonym name2 ts2)  = name1 == name2 && ts1 == ts2
  (==) (ForAll name1 ty1 scope1)        (ForAll name2 ty2 scope2)         = name1 == name2 && ty1 == ty2 && scope1 == scope2
  (==) (ConstrainedType cs1 ty1)        (ConstrainedType cs2 ty2)         = cs1 == cs2 && ty1 == ty2
  (==) (Skolem n1 scope1)               (Skolem n2 scope2)                = n1 == n2 && scope1 == scope2
  (==) REmpty                           REmpty                            = true
  (==) (RCons name1 tx1 ty1)            (RCons name2 tx2 ty2)             = name1 == name2 && tx1 == tx2 && ty1 == ty2
  (==) (PrettyPrintFunction tx1 ty1)    (PrettyPrintFunction tx2 ty2)     = tx1 == tx2 && ty1 == ty2
  (==) (PrettyPrintArray t1)            (PrettyPrintArray t2)             = t1 == t2
  (==) (PrettyPrintObject t1)           (PrettyPrintObject t2)            = t1 == t2
  (==) (PrettyPrintForAll q1 t1)        (PrettyPrintForAll q2 t2)         = q1 == q2 && t1 == t2
  (==) _ _ = false
  (/=) s1 s2 = not (s1 == s2)

-- |
-- Convert a row to a list of pairs of labels and types
--
rowToList :: Type -> Tuple [Tuple String Type] Type
rowToList (RCons name ty row) = case rowToList row of
  (Tuple tys rest) -> Tuple (Tuple name ty : tys) rest
rowToList r = Tuple [] r

-- |
-- Convert a list of labels and types to a row
--
rowFromList :: Tuple [Tuple String Type] Type -> Type
rowFromList (Tuple [] r) = r
rowFromList (Tuple (Tuple name t : ts) r) = RCons name t (rowFromList (Tuple ts r))

-- |
-- Check whether a type is a monotype
--
isMonoType :: Type -> Boolean
isMonoType (ForAll _ _ _) = false
isMonoType _ = true

-- |
-- Universally quantify a type
--
mkForAll :: [String] -> Type -> Type
mkForAll args ty = foldl (\t arg -> ForAll arg t Nothing) ty args

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars = replaceTypeVars' []
  where
  replaceTypeVars' bound name replacement = go bound
    where
    go :: [String] -> Type -> Type
    go _  (TypeVar v) | v == name = replacement
    go bs (TypeApp t1 t2) = TypeApp (go bs t1) (go bs t2)
    go bs (SaturatedTypeSynonym name' ts) = SaturatedTypeSynonym name' $ map (go bs) ts
    go bs f@(ForAll v t sco) | v == name = f
    go bs f@(ForAll v t sco) | v `elem` usedTypeVariables replacement =
                                 let v' = genName v (name : bs ++ usedTypeVariables replacement)
                                     t' = replaceTypeVars' bs v (TypeVar v') t
                                 in ForAll v' (go (v' : bs) t') sco
    go bs f@(ForAll v t sco) = ForAll v (go (v : bs) t) sco
    go bs (ConstrainedType cs t) = ConstrainedType (map (second $ map (go bs)) cs) (go bs t)
    go bs (RCons name' t r) = RCons name' (go bs t) (go bs r)
    go _ ty = ty
  genName orig inUse = try 0
    where
    try :: Number -> String
    try n | (orig ++ show n) `elem` inUse = try (n + 1)
    try n = orig ++ show n

-- |
-- Replace named type variables with types
--
replaceAllTypeVars :: [Tuple String Type] -> Type -> Type
replaceAllTypeVars = foldl (\f (Tuple name ty) -> replaceTypeVars name ty <<< f) id

-- |
-- Collect all type variables appearing in a type
--
usedTypeVariables :: Type -> [String]
usedTypeVariables = nub <<< everythingOnTypes (++) go
  where
  go (TypeVar v) = [v]
  go _ = []

-- |
-- Collect all free type variables appearing in a type
--
freeTypeVariables :: Type -> [String]
freeTypeVariables = nub <<< go []
  where
  go :: [String] -> Type -> [String]
  go bound (TypeVar v) | v `notElem` bound = [v]
  go bound (TypeApp t1 t2) = go bound t1 ++ go bound t2
  go bound (SaturatedTypeSynonym _ ts) = concatMap (go bound) ts
  go bound (ForAll v t _) = go (v : bound) t
  go bound (ConstrainedType cs t) = concatMap (concatMap (go bound) <<< snd) cs ++ go bound t
  go bound (RCons _ t r) = go bound t ++ go bound r
  go _ _ = []

-- |
-- Universally quantify over all type variables appearing free in a type
--
quantify :: Type -> Type
quantify ty = foldr (\arg t -> ForAll arg t Nothing) ty $ freeTypeVariables ty

-- |
-- Move all universal quantifiers to the front of a type
--
moveQuantifiersToFront :: Type -> Type
moveQuantifiersToFront = go [] []
  where
  go qs cs (ForAll q ty sco) = go (Tuple q sco : qs) cs ty
  go qs cs (ConstrainedType cs' ty) = go qs (cs ++ cs') ty
  go qs cs ty =
    let constrained = case cs of
                        [] -> ty
                        cs' -> ConstrainedType cs' ty
    in case qs of
         [] -> constrained
         qs' -> foldl (\ty' (Tuple q sco) -> ForAll q ty' sco) constrained qs'

--
-- Traversals
--

everywhereOnTypes :: (Type -> Type) -> Type -> Type
everywhereOnTypes f = go
  where
  go (TypeApp t1 t2) = f (TypeApp (go t1) (go t2))
  go (SaturatedTypeSynonym name tys) = f (SaturatedTypeSynonym name (map go tys))
  go (ForAll arg ty sco) = f (ForAll arg (go ty) sco)
  go (ConstrainedType cs ty) = f (ConstrainedType (map ((<$>) (map go)) cs) (go ty))
  go (RCons name ty rest) = f (RCons name (go ty) (go rest))
  go (PrettyPrintFunction t1 t2) = f (PrettyPrintFunction (go t1) (go t2))
  go (PrettyPrintArray t) = f (PrettyPrintArray (go t))
  go (PrettyPrintObject t) = f (PrettyPrintObject (go t))
  go (PrettyPrintForAll args t) = f (PrettyPrintForAll args (go t))
  go other = f other

everywhereOnTypesTopDown :: (Type -> Type) -> Type -> Type
everywhereOnTypesTopDown f = go <<< f
  where
  go (TypeApp t1 t2) = TypeApp (go (f t1)) (go (f t2))
  go (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym name (map (go <<< f) tys)
  go (ForAll arg ty sco) = ForAll arg (go (f ty)) sco
  go (ConstrainedType cs ty) = ConstrainedType (map ((<$>) (map (go <<< f))) cs) (go (f ty))
  go (RCons name ty rest) = RCons name (go (f ty)) (go (f rest))
  go (PrettyPrintFunction t1 t2) = PrettyPrintFunction (go (f t1)) (go (f t2))
  go (PrettyPrintArray t) = PrettyPrintArray (go (f t))
  go (PrettyPrintObject t) = PrettyPrintObject (go (f t))
  go (PrettyPrintForAll args t) = PrettyPrintForAll args (go (f t))
  go other = f other

{-
everywhereOnTypesM :: forall m. (Traversable m, Monad m) => (Type -> m Type) -> Type -> m Type
everywhereOnTypesM f = go
  where
  go (TypeApp t1 t2) = (TypeApp <$> go t1 <*> go t2) >>= f
  go (SaturatedTypeSynonym name tys) = (SaturatedTypeSynonym name <$> traverse go tys) >>= f
  go (ForAll arg ty sco) = (ForAll arg <$> go ty <*> pure sco) >>= f
  go (ConstrainedType cs ty) = (ConstrainedType <$> traverse (sndM (traverse go)) cs <*> go ty) >>= f
  go (RCons name ty rest) = (RCons name <$> go ty <*> go rest) >>= f
  go (PrettyPrintFunction t1 t2) = (PrettyPrintFunction <$> go t1 <*> go t2) >>= f
  go (PrettyPrintArray t) = (PrettyPrintArray <$> go t) >>= f
  go (PrettyPrintObject t) = (PrettyPrintObject <$> go t) >>= f
  go (PrettyPrintForAll args t) = (PrettyPrintForAll args <$> go t) >>= f
  go other = f other

everywhereOnTypesTopDownM :: forall m. (Traversable m, Monad m) => (Type -> m Type) -> Type -> m Type
everywhereOnTypesTopDownM f = go <=< f
  where
  go (TypeApp t1 t2) = TypeApp <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym name <$> traverse (go <=< f) tys
  go (ForAll arg ty sco) = ForAll arg <$> (f ty >>= go) <*> pure sco
  go (ConstrainedType cs ty) = ConstrainedType <$> traverse (sndM (traverse (go <=< f))) cs <*> (f ty >>= go)
  go (RCons name ty rest) = RCons name <$> (f ty >>= go) <*> (f rest >>= go)
  go (PrettyPrintFunction t1 t2) = PrettyPrintFunction <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (PrettyPrintArray t) = PrettyPrintArray <$> (f t >>= go)
  go (PrettyPrintObject t) = PrettyPrintObject <$> (f t >>= go)
  go (PrettyPrintForAll args t) = PrettyPrintForAll args <$> (f t >>= go)
  go other = f other
-}

everythingOnTypes :: forall r. (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes (<>) f = go
  where
  go t@(TypeApp t1 t2) = f t <> go t1 <> go t2
  go t@(SaturatedTypeSynonym _ tys) = foldl (<>) (f t) (map go tys)
  go t@(ForAll _ ty _) = f t <> go ty
  go t@(ConstrainedType cs ty) = foldl (<>) (f t) (map go $ concatMap snd cs) <> go ty
  go t@(RCons _ ty rest) = f t <> go ty <> go rest
  go t@(PrettyPrintFunction t1 t2) = f t <> go t1 <> go t2
  go t@(PrettyPrintArray t1) = f t <> go t1
  go t@(PrettyPrintObject t1) = f t <> go t1
  go t@(PrettyPrintForAll _ t1) = f t <> go t1
  go other = f other
