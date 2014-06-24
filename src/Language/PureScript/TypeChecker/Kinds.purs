module Language.PureScript.TypeChecker.Kinds
  ( kindOf
  , kindsOf
  , kindsOfAll
  ) where

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Kinds
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

import Control.Monad (replicateM)
import Control.Monad.Error
import Control.Monad.Error.Proxy

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception

import Data.Array (length, map, zipWith)
import Data.Array.Unsafe (head)
import Data.Foldable (foldr, foldl, for_)
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse, zipWithA)
import Data.Tuple
import Data.Tuple3
import qualified Data.Map as M

instance partialKind :: Partial Kind where
  unknown = KUnknown
  isUnknown (KUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns = everythingOnKinds (++) go
    where
    go (KUnknown u) = [u]
    go _ = []
  ($?) sub = everywhereOnKinds go
    where
    go t@(KUnknown u) = case M.lookup u (runSubstitution sub) of
                          Nothing -> t
                          Just t' -> t'
    go other = other

instance unifiableCheckKind :: Unifiable Kind where
  unify _     (KUnknown u1) (KUnknown u2) | u1 == u2 = return unit
  unify stRef (KUnknown u) k = substitute stRef u k
  unify stRef k (KUnknown u) = substitute stRef u k
  unify _     Star Star = return unit
  unify _     Bang Bang = return unit
  unify stRef (Row k1) (Row k2) = unify stRef k1 k2
  unify stRef (FunKind k1 k2) (FunKind k3 k4) = do
    unify stRef k1 k3
    unify stRef k2 k4
  unify _ k1 k2 = throwException $ strMsg $ "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2 ++ "."

-- |
-- Infer the kind of a single type
--
kindOf :: RefVal CheckState -> ModuleName -> Type -> Check Kind
kindOf chSt _ ty =
  rethrowException (\x -> mkErrorStack "Error checking kind" (Just (TypeError ty)) <> x) $
    tidyUp <$> withSubstitution chSt (\stRef -> starIfUnknown <$> infer chSt stRef ty)
  where
  tidyUp (Tuple k sub) = sub $? k

-- |
-- Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
--
kindsOf :: RefVal CheckState -> Boolean -> ModuleName -> ProperName -> [String] -> [Type] -> Check Kind
kindsOf chSt isData moduleName name args ts = tidyUp <$> withSubstitution chSt (\stRef -> do
  tyCon <- fresh stRef
  kargs <- replicateM (length args) (fresh stRef)
  let dict = (Tuple name tyCon) : zipWith (\arg kind -> (Tuple arg kind)) (map ProperName args) kargs
  bindLocalTypeVariables chSt moduleName dict $ solveTypes chSt stRef isData ts kargs tyCon)
  where
  tidyUp (Tuple k sub) = starIfUnknown (sub $? k)

-- |
-- Simultaneously infer the kinds of several mutually recursive type constructors
--
kindsOfAll :: RefVal CheckState -> ModuleName -> [Tuple3 ProperName [String] Type] -> [Tuple3 ProperName [String] [Type]] -> Check (Tuple [Kind] [Kind])
kindsOfAll chSt moduleName syns tys = tidyUp <$> withSubstitution chSt (\stRef -> do
  synVars <- replicateM (length syns) (fresh stRef)
  let dict = zipWith (\(Tuple3 name _ _) var -> (Tuple name var)) syns synVars
  bindLocalTypeVariables chSt moduleName dict $ do
    tyCons <- replicateM (length tys) (fresh stRef)
    let dict' = zipWith (\(Tuple3 name _ _) tyCon -> (Tuple name tyCon)) tys tyCons
    bindLocalTypeVariables chSt moduleName dict' $ do
      data_ks <- zipWithA (\tyCon (Tuple3 _ args ts) -> do
        kargs <- replicateM (length args) (fresh stRef)
        let argDict = zip (map ProperName args) kargs
        bindLocalTypeVariables chSt moduleName argDict $
          solveTypes chSt stRef true ts kargs tyCon) tyCons tys
      syn_ks <- zipWithA (\synVar (Tuple3 _ args ty) -> do
        kargs <- replicateM (length args) (fresh stRef)
        let argDict = zip (map ProperName args) kargs
        bindLocalTypeVariables chSt moduleName argDict $
          solveTypes chSt stRef false [ty] kargs synVar) synVars syns
      return (Tuple syn_ks data_ks))
  where
  tidyUp (Tuple (Tuple ks1 ks2) sub) = Tuple (map (starIfUnknown <<< (\x -> sub $? x)) ks1) (map (starIfUnknown <<< (\x -> sub $? x)) ks2)

-- |
-- Solve the set of kind constraints associated with the data constructors for a type constructor
--
solveTypes :: RefVal CheckState -> RefVal (UnifyState Kind) -> Boolean -> [Type] -> [Kind] -> Kind -> Check Kind
solveTypes chSt stRef isData ts kargs tyCon = traverse (infer chSt stRef) ts >>= \ks ->
  if isData 
  then do
    unify stRef tyCon (foldr FunKind Star kargs)
    for_ ks $ \k -> unify stRef k Star
    return tyCon
  else do
    unify stRef tyCon (foldr FunKind (head ks) kargs)
    return tyCon

-- |
-- Default all unknown kinds to the Star kind of types
--
starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (Row k) = Row (starIfUnknown k)
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

-- |
-- Infer a kind for a type
--
infer :: RefVal CheckState -> RefVal (UnifyState Kind) -> Type -> Check Kind
infer chSt stRef ty = rethrowException (\x -> mkErrorStack "Error inferring type of value" (Just (TypeError ty)) <> x) $ infer' chSt stRef ty

infer' :: RefVal CheckState -> RefVal (UnifyState Kind) -> Type -> Check Kind
infer' chSt stRef (TypeVar v) = do
  Just moduleName <- getCurrentModule chSt
  lookupTypeVariable chSt moduleName (Qualified Nothing (ProperName v))
infer' chSt stRef c@(TypeConstructor v) = do
  Environment env <- getEnv chSt
  case M.lookup v env.types of
    Nothing -> throwException $ mkErrorStack "Unknown type constructor" (Just (TypeError c))
    Just (Tuple kind _) -> return kind
infer' chSt stRef (TypeApp t1 t2) = do
  k0 <- fresh stRef
  k1 <- infer chSt stRef t1
  k2 <- infer chSt stRef t2
  unify stRef k1 $ FunKind k2 k0
  return k0
infer' chSt stRef (ForAll ident ty _) = do
  k1 <- fresh stRef
  Just moduleName <- getCurrentModule chSt
  k2 <- bindLocalTypeVariables chSt moduleName [Tuple (ProperName ident) k1] $ infer chSt stRef ty
  unify stRef k2 Star
  return Star
infer' chSt stRef REmpty = do
  k <- fresh stRef
  return $ Row k
infer' chSt stRef (RCons _ ty row) = do
  k1 <- infer chSt stRef ty
  k2 <- infer chSt stRef row
  unify stRef k2 (Row k1)
  return $ Row k1
infer' chSt stRef (ConstrainedType deps ty) = do
  for_ deps $ \(Tuple className tys) -> do
    _ <- infer chSt stRef $ foldl TypeApp (TypeConstructor className) tys
    return unit
  k <- infer chSt stRef ty
  unify stRef k Star
  return Star
infer' _ _ _ = theImpossibleHappened "Invalid argument to infer"
