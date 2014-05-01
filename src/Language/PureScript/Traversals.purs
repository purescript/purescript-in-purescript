-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Traversals
-- Copyright   :  (c) 2014 Phil Freeman
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Common functions for implementing generic traversals
--
-----------------------------------------------------------------------------

module Language.PureScript.Traversals where

import Data.Maybe
import Data.Tuple
import Data.Tuple3
import Control.Apply

fstM :: forall f a b c. (Functor f) => (a -> f c) -> (Tuple a b) -> f (Tuple c b)
fstM f (Tuple a b) = flip Tuple b <$> f a

sndM :: forall f a b c. (Functor f) => (b -> f c) -> (Tuple a b) -> f (Tuple a c)
sndM f (Tuple a b) = Tuple a <$> f b

thirdM :: forall f a b c d. (Functor f) => (c -> f d) -> (Tuple3 a b c) -> f (Tuple3 a b d)
thirdM f (Tuple3 a b c) = Tuple3 a b <$> f c

maybeM :: forall f a b. (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
maybeM _ Nothing = pure Nothing
maybeM f (Just a) = Just <$> f a

defS :: forall m st val. (Monad m) => st -> val -> m (Tuple st val)
defS s val = return (Tuple s val)