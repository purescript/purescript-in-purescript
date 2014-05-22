module Language.PureScript.Supply where

import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Error.Class
import Control.Monad.Trans
import Data.Tuple

data SupplyT m a = SupplyT (StateT Number m a)

unSupplyT :: forall m a. SupplyT m a -> StateT Number m a
unSupplyT (SupplyT s) = s

runSupplyT :: forall m a. Number -> SupplyT m a -> m (Tuple a Number)
runSupplyT n = flip runStateT n <<< unSupplyT

evalSupplyT :: forall m a. (Functor m) => Number -> SupplyT m a -> m a
evalSupplyT n = (<$>) fst <<< runSupplyT n

type Supply = SupplyT Identity

runSupply :: forall a. Number -> Supply a -> (Tuple a Number)
runSupply n = runIdentity <<< runSupplyT n

evalSupply :: forall a. Number -> Supply a -> a
evalSupply n = runIdentity <<< evalSupplyT n

fresh :: forall m. (Monad m) => SupplyT m Number
fresh = SupplyT $ do
  n <- get
  put (n + 1)
  return n

freshName :: forall m. (Monad m) => SupplyT m String
freshName = mkName <$> fresh
  where
  mkName n = "_" ++ show n

instance functorSupplyT :: (Monad m) => Functor (SupplyT m) where
  (<$>) f (SupplyT x) = SupplyT (f <$> x)

instance applySupplyT :: (Monad m) => Apply (SupplyT m) where
  (<*>) (SupplyT f) (SupplyT x) = SupplyT (f <*> x)

instance applicativeSupplyT :: (Monad m) => Applicative (SupplyT m) where
  pure a = SupplyT (pure a)

instance bindSupplyT :: (Monad m) => Bind (SupplyT m) where
  (>>=) (SupplyT x) f = SupplyT (x >>= unSupplyT <<< f)

instance monadSupplyT :: (Monad m) => Monad (SupplyT m)

instance monadTransSupplyT :: MonadTrans SupplyT where
  lift = SupplyT <<< lift

instance monadErrorSupplyT :: (Monad m, MonadError e m) => MonadError e (SupplyT m) where
  throwError = SupplyT <<< throwError
  catchError e f = SupplyT $ catchError (unSupplyT e) (unSupplyT <<< f)
