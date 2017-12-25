{-# LANGUAGE LambdaCase, FlexibleContexts #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.awaitery.Glass
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Things to work with finite streams
-----------------------------------------------------------------------
module Data.Drinkery.Glass
  ( eof
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
eof :: (Applicative m, Alternative f) => Tap r (f a) m
eof = Tap $ const $ pure (empty, eof)

-- | Run a 'Barman' action and terminate the stream with 'eof'.
runBarman :: (Monoid r, Applicative m, Alternative f) => Barman r (f s) m a -> Tap r (f s) m
runBarman m = unBarman m (const eof)
{-# INLINE runBarman #-}

-- | Run 'Sommelier' and terminate the stream with 'eof'.
runSommelier :: (Monoid r, Applicative m, Alternative f) => Sommelier r m s -> Tap r (f s) m
runSommelier m = unSommelier m (consTap . pure) eof
{-# INLINE runSommelier #-}

pour :: (Monoid r, Applicative f, Applicative m) => s -> Barman r (f s) m ()
pour = yield . pure
{-# INLINE pour #-}

foldl' :: (Foldable t, Monoid r, MonadDrunk (Tap r (t a)) m) => (b -> a -> b) -> b -> m b
foldl' f = go where
  go b = do
    t <- await
    if null t
      then return b
      else go $! F.foldl' f b t
{-# INLINE foldl' #-}

foldM :: (Foldable t, Monoid r, MonadDrunk (Tap r (t a)) m) => (b -> a -> m b) -> b -> m b
foldM f = go where
  go b = do
    t <- await
    if null t
      then return b
      else F.foldlM f b t >>= go
{-# INLINE foldM #-}

traverse_ :: (Foldable t, Monoid r, MonadDrunk (Tap r (t a)) m) => (a -> m b) -> m ()
traverse_ f = go where
  go = do
    t <- await
    if null t then return () else F.traverse_ f t >> go
{-# INLINE traverse_ #-}

sinkNull :: (Foldable t, Monoid r, MonadDrunk (Tap r (t a)) m) => m ()
sinkNull = go where
  go = await >>= \t -> unless (null t) go
{-# INLINE sinkNull #-}
