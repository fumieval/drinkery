{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Glass
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Things to work with finite streams
-----------------------------------------------------------------------
module Data.Drinkery.Glass
  ( digestif
  , runBarman
  , runSommelier
  , pour
  -- * 'MonadDrunk' actions
  , foldl'
  , foldM
  , traverse_
  , sinkNull
  )
where

import Control.Applicative
import Control.Monad hiding (foldM)
import Data.Drinkery.Class
import Data.Drinkery.Tap
import qualified Data.Foldable as F

-- | End of stream
digestif :: (Applicative m, Alternative f) => Tap m r (f a)
digestif = Tap $ const $ pure (empty, digestif)

-- | Run a 'Barman' action and terminate the stream with 'digestif'.
runBarman :: (Monoid r, Applicative m, Alternative f) => Barman r (f s) m a -> Tap m r (f s)
runBarman m = unBarman m (const digestif)
{-# INLINE runBarman #-}

-- | Run 'Sommelier' and terminate the stream with 'digestif'.
runSommelier :: (Monoid r, Applicative m, Alternative f) => Sommelier r m s -> Tap m r (f s)
runSommelier m = unSommelier m (consTap . pure) digestif
{-# INLINE runSommelier #-}

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
