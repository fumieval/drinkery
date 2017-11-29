{-# LANGUAGE LambdaCase #-}
module Data.Drinkery.Glass where

import Control.Applicative
import Control.Monad
import Data.Drinkery.Boozer
import Data.Drinkery.Class
import Data.Drinkery.Tap
import Data.Foldable as F

digestif :: (Applicative m, Alternative f) => Tap m r (f a)
digestif = Tap $ const $ pure (empty, digestif)

serve :: (Applicative m, Alternative f) => Barman r (f s) m x -> Tap m r (f s)
serve t = unBarman t (const digestif)

pour :: (Monoid r, Applicative f, Applicative m) => s -> Barman r (f s) m ()
pour = topup . pure

foldl' :: (Foldable t, MonadDrunk r (t a) m) => (b -> a -> b) -> b -> m b
foldl' f = go where
  go b = do
    t <- drink
    if null t
      then return b
      else go $! F.foldl' f b t

foldM :: (Foldable t, MonadDrunk r (t a) m) => (b -> a -> m b) -> b -> m b
foldM f = go where
  go b = do
    t <- drink
    if null t
      then return b
      else F.foldlM f b t >>= go

traverse_ :: (Foldable t, MonadDrunk r (t a) m) => (a -> m b) -> m ()
traverse_ f = go where
  go = do
    t <- drink
    if null t then return () else F.traverse_ f t >> go

sinkNull :: (Foldable t, MonadDrunk r (t a) m) => m ()
sinkNull = drink >>= \t -> unless (null t) sinkNull

runBarman :: (Monoid r, Applicative m, Alternative f) => Barman r (f s) m a -> Tap m r (f s)
runBarman m = unBarman m (const digestif)
{-# INLINE closing #-}

runSommelier :: (Monoid r, Applicative m, Alternative f) => Sommelier r m s -> Tap m r (f s)
runSommelier m = unSommelier m (consTap . pure) digestif
{-# INLINE recommended #-}
