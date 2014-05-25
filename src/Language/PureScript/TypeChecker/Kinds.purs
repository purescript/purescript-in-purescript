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
import Control.Monad.Error.Class
import Control.Monad.Error.Proxy
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Unify

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

instance unifiableCheckKind :: Unifiable Check Kind where
  (=?=) (KUnknown u1) (KUnknown u2) | u1 == u2 = return {}
  (=?=) (KUnknown u) k = substitute unifyError u k
  (=?=) k (KUnknown u) = substitute unifyError u k
  (=?=) Star Star = return {}
  (=?=) Bang Bang = return {}
  (=?=) (Row k1) (Row k2) = k1 =?= k2
  (=?=) (FunKind k1 k2) (FunKind k3 k4) = do
    k1 =?= k3
    k2 =?= k4
  (=?=) k1 k2 = UnifyT $ lift $ throwError $ withErrorType unifyError $ strMsg $ "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2 ++ "."

-- |
-- Infer the kind of a single type
--
kindOf :: ModuleName -> Type -> Check Kind
kindOf _ ty =
  rethrow (\x -> mkErrorStack "Error checking kind" (Just (TypeError ty)) <> x) $
    (<$>) tidyUp <<< liftUnify $ starIfUnknown <$> infer ty
  where
  tidyUp (Tuple k sub) = sub $? k

-- |
-- Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
--
kindsOf :: Boolean -> ModuleName -> ProperName -> [String] -> [Type] -> Check Kind
kindsOf isData moduleName name args ts = (<$>) tidyUp <<< liftUnify $ do
  tyCon <- fresh
  kargs <- replicateM (length args) fresh
  let dict = (Tuple name tyCon) : zipWith (\arg kind -> (Tuple arg kind)) (map ProperName args) kargs
  bindLocalTypeVariables moduleName dict $ solveTypes isData ts kargs tyCon
  where
  tidyUp (Tuple k sub) = starIfUnknown (sub $? k)

-- |
-- Simultaneously infer the kinds of several mutually recursive type constructors
--
kindsOfAll :: ModuleName -> [Tuple3 ProperName [String] Type] -> [Tuple3 ProperName [String] [Type]] -> Check (Tuple [Kind] [Kind])
kindsOfAll moduleName syns tys = (<$>) tidyUp <<< liftUnify $ do
  synVars <- replicateM (length syns) fresh
  let dict = zipWith (\(Tuple3 name _ _) var -> (Tuple name var)) syns synVars
  bindLocalTypeVariables moduleName dict $ do
    tyCons <- replicateM (length tys) fresh
    let dict' = zipWith (\(Tuple3 name _ _) tyCon -> (Tuple name tyCon)) tys tyCons
    bindLocalTypeVariables moduleName dict' $ do
      data_ks <- zipWithA (\tyCon (Tuple3 _ args ts) -> do
        kargs <- replicateM (length args) fresh
        let argDict = zip (map ProperName args) kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes true ts kargs tyCon) tyCons tys
      syn_ks <- zipWithA (\synVar (Tuple3 _ args ty) -> do
        kargs <- replicateM (length args) fresh
        let argDict = zip (map ProperName args) kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes false [ty] kargs synVar) synVars syns
      return (Tuple syn_ks data_ks)
  where
  tidyUp (Tuple (Tuple ks1 ks2) sub) = Tuple (map (starIfUnknown <<< (\x -> sub $? x)) ks1) (map (starIfUnknown <<< (\x -> sub $? x)) ks2)

-- |
-- Solve the set of kind constraints associated with the data constructors for a type constructor
--
solveTypes :: Boolean -> [Type] -> [Kind] -> Kind -> UnifyT Kind (Check) Kind
solveTypes isData ts kargs tyCon = traverse infer ts >>= \ks ->
  if isData 
  then do
    tyCon =?= foldr FunKind Star kargs
    for_ ks $ \k -> k =?= Star
    return tyCon
  else do
    tyCon =?= foldr FunKind (head ks) kargs
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
infer :: Type -> UnifyT Kind Check Kind
infer ty = rethrow (\x -> mkErrorStack "Error inferring type of value" (Just (TypeError ty)) <> x) $ infer' ty

infer' :: Type -> UnifyT Kind Check Kind
infer' (TypeVar v) = do
  Just moduleName <- (\(CheckState st) -> st.currentModule) <$> get
  UnifyT $ lift $ lookupTypeVariable unifyError moduleName (Qualified Nothing (ProperName v))
infer' c@(TypeConstructor v) = do
  Environment env <- liftCheck getEnv
  case M.lookup v env.types of
    Nothing -> UnifyT $ lift $ throwError $ mkErrorStack "Unknown type constructor" (Just (TypeError c))
    Just (Tuple kind _) -> return kind
infer' (TypeApp t1 t2) = do
  k0 <- fresh
  k1 <- infer t1
  k2 <- infer t2
  k1 =?= FunKind k2 k0
  return k0
infer' (ForAll ident ty _) = do
  k1 <- fresh
  Just moduleName <- (\(CheckState st) -> st.currentModule) <$> get
  k2 <- bindLocalTypeVariables moduleName [Tuple (ProperName ident) k1] $ infer ty
  k2 =?= Star
  return Star
infer' REmpty = do
  k <- fresh
  return $ Row k
infer' (RCons _ ty row) = do
  k1 <- infer ty
  k2 <- infer row
  k2 =?= Row k1
  return $ Row k1
infer' (ConstrainedType deps ty) = do
  for_ deps $ \(Tuple className tys) -> do
    _ <- infer $ foldl TypeApp (TypeConstructor className) tys
    return {}
  k <- infer ty
  k =?= Star
  return Star
infer' _ = error "Invalid argument to infer"
