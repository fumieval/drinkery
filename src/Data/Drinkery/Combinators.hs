{-# LANGUAGE LambdaCase, FlexibleContexts #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Combinators
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------
module Data.Drinkery.Combinators
  ( foldlFrom'
  , foldMFrom
  , traverseFrom_
  , drainFrom
  )
where

import Control.Monad hiding (foldM)
import qualified Data.Foldable as F

foldlFrom' :: (Foldable t, Monad m) => m (t a) -> (b -> a -> b) -> b -> m b
foldlFrom' m f = go where
  go b = do
    t <- m
    if null t
      then return b
      else go $! F.foldl' f b t
{-# INLINE foldlFrom' #-}

foldMFrom :: (Foldable t, Monad m) => m (t a) -> (b -> a -> m b) -> b -> m b
foldMFrom m f = go where
  go b = do
    t <- m
    if null t
      then return b
      else F.foldlM f b t >>= go
{-# INLINE foldMFrom #-}

traverseFrom_ :: (Foldable t, Monad m) => m (t a) -> (a -> m b) -> m ()
traverseFrom_ m f = go where
  go = do
    t <- m
    if null t then return () else F.traverse_ f t >> go
{-# INLINE traverseFrom_ #-}

drainFrom :: (Foldable t, Monad m) => m (t a) -> m ()
drainFrom m = go where
  go = m >>= \t -> unless (null t) go
{-# INLINE drainFrom #-}
