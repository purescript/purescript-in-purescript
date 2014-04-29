-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Unify
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
--
-----------------------------------------------------------------------------

module Control.Monad.Unify where

import Data.Either
import Data.Foldable
import Data.Generics
import Data.Maybe
import Data.Monoid
import Data.Tuple

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.State.Class
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class

-- |
-- Untyped unification variables
--
data Unknown = Unknown Number

runUnknown :: Unknown -> Number
runUnknown (Unknown u) = u

instance genericUnknown :: Generic Unknown where
  typeOf _ = TyCon { tyCon: "Control.Monad.Unify.Unknown", args: [] }
  term (Unknown u) = TmCon { con: "Control.Monad.Unify.Unknown", values : [term u] }
  unTerm (TmCon { con = "Control.Monad.Unify.Unknown", values = [u] }) = Unknown <$> unTerm u
  unTerm _ = Nothing

instance showUnknown :: Show Unknown where
  show (Unknown u) = "Unknown " ++ show u

instance eqUnknown :: Eq Unknown where
  (==) (Unknown u1) (Unknown u2) = u1 == u2
  (/=) (Unknown u1) (Unknown u2) = u1 /= u2

instance ordUnknown :: Ord Unknown where
  compare (Unknown u1) (Unknown u2) = compare u1 u2

-- |
-- A type which can contain unification variables
--
class Partial t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown

-- |
-- Identifies types which support unification
--
class Unifiable m t where
  (=?=) :: t -> t -> UnifyT t m {}

-- |
-- A substitution maintains a mapping from unification variables to their values
--
data Substitution t = Substitution (Data.Map.Map Number t)

runSubstitution :: forall t. Substitution t -> Data.Map.Map Number t
runSubstitution (Substitution m) = m

instance semigroupSubstitution :: (Generic t, Partial t) => Semigroup (Substitution t) where
  (<>) s1 s2 = Substitution $
                 Data.Map.map (($?) s2) (runSubstitution s1) `Data.Map.union`
                 Data.Map.map (($?) s1) (runSubstitution s2)

instance monoidSubstitution :: (Generic t, Partial t) => Monoid (Substitution t) where
  mempty = Substitution Data.Map.empty

-- |
-- Apply a substitution to a value
--
($?) :: forall t. (Generic t, Partial t) => Substitution t -> t -> t
($?) sub = everywhere (mkT go)
  where
  go t = case isUnknown t of
    Nothing -> t
    Just (Unknown u) -> 
      case Data.Map.lookup u (runSubstitution sub) of
        Nothing -> t
        Just t' -> t'

-- |
-- State required for type checking
--
data UnifyState t = UnifyState {
  -- |
  -- The next fresh unification variable
  --
    unifyNextVar :: Number
  -- |
  -- The current substitution
  --
  , unifyCurrentSubstitution :: Substitution t
  }

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: forall t. (Generic t, Partial t) => UnifyState t
defaultUnifyState = UnifyState { unifyNextVar: 0, unifyCurrentSubstitution: mempty }

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
data UnifyT t m a = UnifyT (StateT (UnifyState t) (ErrorT String m) a)

unUnifyT :: forall t m a. UnifyT t m a -> StateT (UnifyState t) (ErrorT String m) a
unUnifyT (UnifyT s) = s

instance functorUnify :: (Monad m) => Functor (UnifyT t m) where
  (<$>) = liftA1

instance applyUnify :: (Monad m) => Apply (UnifyT t m) where
  (<*>) = ap

instance applicativeUnify :: (Monad m) => Applicative (UnifyT t m) where
  pure = UnifyT <<< pure

instance bindUnify :: (Monad m) => Bind (UnifyT t m) where
  (>>=) (UnifyT x) f = UnifyT (x >>= unUnifyT <<< f)
  
instance monadUnify :: (Monad m) => Monad (UnifyT t m)
  
instance monadErrorUnify :: (Monad m) => MonadError String (UnifyT t m) where
  throwError = UnifyT <<< throwError
  catchError (UnifyT a) f = UnifyT (a `catchError` (unUnifyT <<< f))

instance monadStateUnify :: (Monad m, MonadState s m) => MonadState s (UnifyT t m) where
  state f = UnifyT (lift (lift (state f)))

instance monadStateUnifyState :: (Monad m) => MonadState (UnifyState t) (UnifyT t m) where
  state = UnifyT <<< state

-- |
-- Collect all unknowns occurring inside a value
--
unknowns :: forall d. (Generic d) => d -> [Unknown]
unknowns = 
  let collect (u@(Unknown _)) = [u]
  in everything (++) (mkQ [] collect)

-- |
-- Run a computation in the Unify monad, failing with an error, or succeeding with a return value and the new next unification variable
--
runUnify :: forall t m a. UnifyState t -> UnifyT t m a -> m (Either String (Tuple a (UnifyState t)))
runUnify s = runErrorT <<< flip runStateT s <<< unUnifyT

-- |
-- Substitute a single unification variable
--
substituteOne :: forall t. (Partial t) => Unknown -> t -> Substitution t
substituteOne (Unknown u) t = Substitution $ Data.Map.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
--
(=:=) :: forall m t. (Monad m, Generic t, Partial t, Unifiable m t) => Unknown -> t -> UnifyT t m {}
(=:=) u t' = do
  UnifyState st <- get
  let sub = st.unifyCurrentSubstitution
  let t = sub $? t'
  occursCheck u t
  let current = sub $? unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return {}
    _ -> current =?= t
  modify $ \(UnifyState s) -> UnifyState { unifyNextVar: st.unifyNextVar, unifyCurrentSubstitution: substituteOne u t <> s.unifyCurrentSubstitution }

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: forall m t. (Monad m, Generic t, Partial t) => Unknown -> t -> UnifyT t m {}
occursCheck u t =
  case isUnknown t of
    Nothing | u `elem` unknowns t -> UnifyT (lift (throwError "Occurs check fails"))
    _ -> return {}
    
foreign import undefined :: forall a. a

-- |
-- Generate a fresh untyped unification variable
--
fresh' :: forall m t. (Monad m) => UnifyT t m Unknown
fresh' = undefined
{-do
  UnifyState st <- get
  put $ UnifyState 
    { unifyNextVar: st.unifyNextVar + 1
    , unifyCurrentSubstitution: st.unifyCurrentSubstitution 
    }
  return $ Unknown st.unifyNextVar-}

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: forall m t. (Monad m, Partial t) => UnifyT t m t
fresh = do
  u <- fresh'
  return $ unknown u
