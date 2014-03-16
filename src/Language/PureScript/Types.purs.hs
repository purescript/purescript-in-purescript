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

import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Generics
import Data.Foldable

-- import Control.Monad.Unify

import Control.Arrow (second)

import Language.PureScript.Names

-- |
-- An identifier for the scope of a skolem variable
--
data SkolemScope = SkolemScope Number

runSkolemScope :: SkolemScope -> Number
runSkolemScope (SkolemScope n) = n

instance genericSkolemScope :: Generic SkolemScope where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Types.SkolemScope", args: [] }  
  term (SkolemScope n) = TmCon { con: "Language.PureScript.Types.SkolemScope", values: [term n] }
  unTerm (TmCon { con = "Language.PureScript.Types.SkolemScope", values = [t] }) = SkolemScope <$> unTerm t
  unTerm _ = Nothing

instance showSkolemScope :: Show SkolemScope where
  show = gshow

instance eqSkolemScope :: Eq SkolemScope where
  (==) = geq
  (/=) s1 s2 = not (s1 == s2)

-- TODO: replace when monad-unify is ported
type Unknown = Number

-- |
-- The type of types
--
data Type
  -- |
  -- A unification variable of type Type
  --
  = TUnknown Unknown
  -- |
  -- Javascript numbers
  --
  | Object Type
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
  | PrettyPrintForAll [String] Type

instance genericType :: Generic Type where
  typeOf _ = TyCon { tyCon: "Language.PureScript.Types.Type", args: [] }  
  term (TUnknown u)                         = TmCon { con: "Language.PureScript.Types.TUnknown",             values: [term u] }
  term (Object t)                           = TmCon { con: "Language.PureScript.Types.Object",               values: [term t] }
  term (TypeVar v)                          = TmCon { con: "Language.PureScript.Types.TypeVar",              values: [term v] }
  term (TypeConstructor pn)                 = TmCon { con: "Language.PureScript.Types.TypeConstructor",      values: [term pn] }
  term (TypeApp t1 t2)                      = TmCon { con: "Language.PureScript.Types.TypeApp",              values: [term t1, term t2] }
  term (SaturatedTypeSynonym pn args)       = TmCon { con: "Language.PureScript.Types.SaturatedTypeSynonym", values: [term pn, term args] }
  term (ForAll v t sco)                     = TmCon { con: "Language.PureScript.Types.ForAll",               values: [term v, term t, term sco] }
  term (ConstrainedType cons t)             = TmCon { con: "Language.PureScript.Types.ConstrainedType",      values: [term cons, term t] }
  term (Skolem u sco)                       = TmCon { con: "Language.PureScript.Types.Skolem",               values: [term u, term sco] }
  term REmpty                               = TmCon { con: "Language.PureScript.Types.REmpty",               values: [] }
  term (RCons p t r)                        = TmCon { con: "Language.PureScript.Types.RCons",                values: [term p, term t, term r] }
  term (PrettyPrintFunction t1 t2)          = TmCon { con: "Language.PureScript.Types.PrettyPrintFunction",  values: [term t1, term t2] }
  term (PrettyPrintArray t)                 = TmCon { con: "Language.PureScript.Types.PrettyPrintArray",     values: [term t] }
  term (PrettyPrintForAll vs t)             = TmCon { con: "Language.PureScript.Types.PrettyPrintForAll",    values: [term vs, term t] }
  unTerm (TmCon { con = "Language.PureScript.Types.TUnknown",             values = [u] })              = TUnknown <$> unTerm u
  unTerm (TmCon { con = "Language.PureScript.Types.Object",               values = [t] })              = Object <$> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Types.TypeVar",              values = [v] })              = TypeVar <$> unTerm v
  unTerm (TmCon { con = "Language.PureScript.Types.TypeConstructor",      values = [pn] })             = TypeConstructor <$> unTerm pn
  unTerm (TmCon { con = "Language.PureScript.Types.TypeApp",              values = [t1, t2] })         = TypeApp <$> unTerm t1 <*> unTerm t2
  unTerm (TmCon { con = "Language.PureScript.Types.SaturatedTypeSynonym", values = [pn, args] })       = SaturatedTypeSynonym <$> unTerm pn <*> unTerm args
  unTerm (TmCon { con = "Language.PureScript.Types.ForAll",               values = [v, t, sco] })      = ForAll <$> unTerm v <*> unTerm t <*> unTerm sco
  unTerm (TmCon { con = "Language.PureScript.Types.ConstrainedType",      values = [cons, t] })        = ConstrainedType <$> unTerm cons <*> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Types.Skolem",               values = [u, sco] })         = Skolem <$> unTerm u <*> unTerm sco
  unTerm (TmCon { con = "Language.PureScript.Types.REmpty",               values = [] })               = pure REmpty
  unTerm (TmCon { con = "Language.PureScript.Types.RCons",                values = [p, t, r] })        = RCons <$> unTerm p <*> unTerm t <*> unTerm r
  unTerm (TmCon { con = "Language.PureScript.Types.PrettyPrintFunction",  values = [t1, t2] })         = PrettyPrintFunction <$> unTerm t1 <*> unTerm t2
  unTerm (TmCon { con = "Language.PureScript.Types.PrettyPrintArray",     values = [t] })              = PrettyPrintArray <$> unTerm t
  unTerm (TmCon { con = "Language.PureScript.Types.PrettyPrintForAll",    values = [vs, t] })          = PrettyPrintForAll <$> unTerm vs <*> unTerm t
  unTerm _ = Nothing

instance showType :: Show Type where
  show = gshow

instance eqType :: Eq Type where
  (==) = geq
  (/=) s1 s2 = not (s1 == s2)

-- |
-- Convert a row to a list of pairs of labels and types
--
rowToList :: Type -> Tuple [Tuple String Type] Type
rowToList (RCons name ty row) = let (Tuple tys rest) = rowToList row
                                in (Tuple (Tuple name ty : tys) rest)
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
-- The empty record type
--
unit :: Type
unit = Object REmpty

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars = replaceTypeVars' []

replaceTypeVars' :: [String] -> String -> Type -> Type -> Type
replaceTypeVars' bs name replacement (Object r) = Object $ replaceTypeVars' bs name replacement r
replaceTypeVars' _  name replacement (TypeVar v) | v == name = replacement
replaceTypeVars' bs name replacement (TypeApp t1 t2) = TypeApp (replaceTypeVars' bs name replacement t1) (replaceTypeVars' bs name replacement t2)
replaceTypeVars' bs name replacement (SaturatedTypeSynonym name' ts) = SaturatedTypeSynonym name' $ map (replaceTypeVars' bs name replacement) ts
replaceTypeVars' bs name replacement f@(ForAll v _ _) | v == name = f
replaceTypeVars' bs name replacement (ForAll v t sco) | v `elem` usedTypeVariables replacement =
  let v' = genName v (name : bs `concat` usedTypeVariables replacement) in
  let t' = replaceTypeVars' bs v (TypeVar v') t in
  ForAll v' (replaceTypeVars' (v' : bs) name replacement t') sco
replaceTypeVars' bs name replacement (ForAll v t sco) = ForAll v (replaceTypeVars' (v : bs) name replacement t) sco
replaceTypeVars' bs name replacement (ConstrainedType cs t) = ConstrainedType (map (second $ map (replaceTypeVars' bs name replacement)) cs) (replaceTypeVars' bs name replacement t)
replaceTypeVars' bs name replacement (RCons name' t r) = RCons name' (replaceTypeVars' bs name replacement t) (replaceTypeVars' bs name replacement r)
replaceTypeVars' _ _ _ ty = ty

genName :: String -> [String] -> String
genName orig inUse = genName' orig inUse 0

genName' :: String -> [String] -> Number -> String
genName' orig inUse n | (orig ++ show n) `elem` inUse = genName' orig inUse (n + 1)
genName' orig _ n = orig ++ show n

-- |
-- Replace named type variables with types
--
replaceAllTypeVars :: [Tuple String Type] -> Type -> Type
replaceAllTypeVars = foldl (\f (Tuple name ty) -> replaceTypeVars name ty <<< f) id

-- |
-- Collect all type variables appearing in a type
--
usedTypeVariables :: Type -> [String]
usedTypeVariables = nub <<< everything concat (mkQ [] usedTypeVariables')

usedTypeVariables' :: Type -> [String]
usedTypeVariables' (TypeVar v) = [v]
usedTypeVariables' _ = []

-- |
-- Collect all free type variables appearing in a type
--
freeTypeVariables :: Type -> [String]
freeTypeVariables = nub <<< freeTypeVariables' []

freeTypeVariables' :: [String] -> Type -> [String]
freeTypeVariables' bound (Object r) = freeTypeVariables' bound r
freeTypeVariables' bound (TypeVar v) | v `notElem` bound = [v]
freeTypeVariables' bound (TypeApp t1 t2) = freeTypeVariables' bound t1 `concat` freeTypeVariables' bound t2
freeTypeVariables' bound (SaturatedTypeSynonym _ ts) = concatMap (freeTypeVariables' bound) ts
freeTypeVariables' bound (ForAll v t _) = freeTypeVariables' (v : bound) t
freeTypeVariables' bound (ConstrainedType cs t) = concatMap (concatMap (freeTypeVariables' bound) <<< snd) cs `concat` freeTypeVariables' bound t
freeTypeVariables' bound (RCons _ t r) = freeTypeVariables' bound t `concat` freeTypeVariables' bound r
freeTypeVariables' _ _ = []

-- |
-- Universally quantify over all type variables appearing free in a type
--
quantify :: Type -> Type
quantify ty = foldr (\arg t -> ForAll arg t Nothing) ty $ freeTypeVariables ty
