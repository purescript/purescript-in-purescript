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
import Data.Maybe
import Data.Monoid
import Data.Tuple

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.State.Class
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Proxy

-- |
-- Untyped unification variables
--
type Unknown = Number

-- |
-- A type which can contain unification variables
--
class Partial t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown
  unknowns :: t -> [Unknown]
  ($?) :: Substitution t -> t -> t

-- |
-- Identifies types which support unification
--
class (Partial t) <= Unifiable m t where
  (=?=) :: t -> t -> UnifyT t m {}

-- |
-- A substitution maintains a mapping from unification variables to their values
--
data Substitution t = Substitution (Data.Map.Map Unknown t)

runSubstitution :: forall t. Substitution t -> Data.Map.Map Number t
runSubstitution (Substitution m) = m


instance semigroupSubstitution :: (Partial t) => Semigroup (Substitution t) where
  (<>) s1 s2 = Substitution $
                 Data.Map.map (($?) s2) (runSubstitution s1) `Data.Map.union`
                 Data.Map.map (($?) s1) (runSubstitution s2)

instance monoidSubstitution :: (Partial t) => Monoid (Substitution t) where
  mempty = Substitution Data.Map.empty

-- |
-- State required for type checking
--
data UnifyState t = UnifyState (UnifyStateObj t)

type UnifyStateObj t = {
  -- |
  -- The next fresh unification variable
  --
    nextVar :: Unknown
  -- |
  -- The current substitution
  --
  , currentSubstitution :: Substitution t
  }
  
unifyStateObj :: forall t. UnifyState t -> UnifyStateObj t
unifyStateObj (UnifyState o) = o

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: forall t. (Partial t) => UnifyState t
defaultUnifyState = UnifyState { nextVar: 0, currentSubstitution: mempty }

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
data UnifyT t m a = UnifyT (StateT (UnifyState t) m a)

unUnifyT :: forall t m a. UnifyT t m a -> StateT (UnifyState t) m a
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
  
instance monadErrorUnify :: (Monad m, MonadError e m) => MonadError e (UnifyT t m) where
  throwError = UnifyT <<< throwError
  catchError e f = UnifyT $ catchError (unUnifyT e) (unUnifyT <<< f)

instance monadStateUnify :: (Monad m, MonadState s m) => MonadState s (UnifyT t m) where
  state f = UnifyT (lift (state f))

instance monadStateUnifyState :: (Monad m) => MonadState (UnifyState t) (UnifyT t m) where
  state = UnifyT <<< state

-- |
-- Run a computation in the Unify monad, failing with an error, or succeeding with a return value and the new next unification variable
--
runUnify :: forall t m a. (Monad m) => UnifyState t -> UnifyT t m a -> m (Tuple a (UnifyState t))
runUnify s = flip runStateT s <<< unUnifyT

-- |
-- Substitute a single unification variable
--
substituteOne :: forall t. (Partial t) => Unknown -> t -> Substitution t
substituteOne u t = Substitution $ Data.Map.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
--
substitute :: forall e m t. (Error e, Monad m, MonadError e m, Partial t, Unifiable m t) => WithErrorType e -> Unknown -> t -> UnifyT t m {}
substitute errorType u t' = do
  UnifyState st <- get
  let sub = st.currentSubstitution
  let t = sub $? t'
  occursCheck errorType u t
  let current = sub $? unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return {}
    _ -> current =?= t
  modify $ \(UnifyState s) -> UnifyState { nextVar: st.nextVar, currentSubstitution: substituteOne u t <> s.currentSubstitution }

-- |
-- This type exists to get around a type error caused by the lack of functional dependencies
--
data Proxy e = Proxy

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: forall e m t. (Error e, Monad m, MonadError e m, Partial t) => WithErrorType e -> Unknown -> t -> UnifyT t m {}
occursCheck errorType u t =
  case isUnknown t of
    Nothing | u `elem` unknowns t -> UnifyT $ lift $ throwError $ withErrorType errorType $ strMsg $ "Occurs check fails"
    _ -> return {}
	
-- |
-- Generate a fresh untyped unification variable
--
fresh' :: forall m t. (Monad m) => UnifyT t m Unknown
fresh' = do
  UnifyState st <- getState
  put $ UnifyState 
    { nextVar: st.nextVar + 1
    , currentSubstitution: st.currentSubstitution 
    }
  return st.nextVar
  where
  getState :: forall m t. (Monad m) => UnifyT t m (UnifyState t)
  getState = get

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: forall m t. (Monad m, Partial t) => UnifyT t m t
fresh = do
  u <- fresh'
  return $ unknown u
