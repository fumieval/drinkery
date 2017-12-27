{-# LANGUAGE Rank2Types, BangPatterns, LambdaCase, FlexibleContexts #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Distiller
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Stream transducers
-----------------------------------------------------------------------
module Data.Drinkery.Distiller
  ( Distiller
  -- * Special combinators
  , (+&)
  , ($&)
  -- * Basic combinators
  , (++$)
  , (++&)
  -- * Stock distillers
  , Still
  , scanningMaybe
  , mapping
  , traversing
  , filtering
  , scanning
  ) where

import Control.Monad.Trans
import Data.Drinkery.Tap
import Data.Drinkery.Class

-- | @Distiller tap m r s@ is a stream transducer which has four parameters:
--
-- * @tap@ input
-- * @m@ underlying monad
-- * @r@ request from the downstream
-- * @s@ output
--
-- This is also a 'Tap'.
--
type Distiller tap m r s = Tap r s (Drinker tap m)

infix 6 +&
infixr 7 $&
infixr 7 ++&
infixl 8 ++$

-- | Connect a tap with a Drinker.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap (along with the result).
-- * @&@ Right operand is a Drinker.
(++&) :: (Monad m) => tap m -> Drinker tap m a -> m (tap m, a)
d ++& b = do
  (a, d') <- runDrinker b d
  return (d', a)
{-# INLINE (++&) #-}

-- | Attach a distiller to a tap.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap.
-- * @$@ Right operand is a distiller.
--
(++$) :: (Monad m) => tap m -> Distiller tap m r s -> Tap r s m
t ++$ d = Tap $ \rs -> do
  ((s, d'), t') <- runDrinker (unTap d rs) t
  return (s, t' ++$ d')

-- | Connect a spigot with a Drinker and close the used tap.
(+&) :: (Closable tap, Monad m) => tap m -> Drinker tap m a -> m a
t +& b = do
  (t', a) <- t ++& b
  close t'
  return a
{-# INLINE (+&) #-}

-- | Like '$&&' but discards the used distiller.
--
-- @($&) :: Distiller tap m r s -> Drinker (Tap r s) m a -> Drinker tap m a@
--
($&) :: (Monad m) => tap m -> Drinker tap m a -> m a
t $& b = fmap snd $ t ++& b
{-# INLINE ($&) #-}

-- | Mono in/out
type Still p q m r s = Distiller (Tap p q) m r s

scanningMaybe :: (Monoid r, Monad m) => (b -> a -> b) -> b -> Still r (Maybe a) m r (Maybe b)
scanningMaybe f b0 = consTap (Just b0) $ go b0 where
  go b = Tap $ \r -> Drinker $ \tap -> do
    (m, t') <- unTap tap r
    case m of
      Just a -> let !b' = f b a in return ((Just b', go b'), t')
      Nothing -> return ((Nothing, go b), t')
{-# INLINE scanningMaybe #-}

-- | Create a request-preserving distiller.
propagating :: (Monoid r, Monad m) => Drinker (Tap r a) m (b, Still r a m r b) -> Still r a m r b
propagating m = Tap $ \r -> request r >> m
{-# INLINE propagating #-}

mapping :: (Monoid r, Monad m) => (a -> b) -> Still r a m r b
mapping f = propagating $ await >>= \a -> return (f a, mapping f)

traversing :: (Monoid r, Monad m) => (a -> m b) -> Still r a m r b
traversing f = propagating $ await >>= \a -> lift (f a) >>= \b -> return (b, traversing f)

filtering :: (Monoid r, Monad m) => (a -> Bool) -> Still r a m r a
filtering f = propagating $ await >>= \a -> if f a
  then return (a, filtering f)
  else unTap (filtering f) mempty

scanning :: (Monoid r, Monad m) => (b -> a -> b) -> b -> Still r a m r b
scanning f b0 = consTap b0 $ go b0 where
  go b = propagating $ fmap (\a -> let !b' = f b a in (b', go $ b')) await
{-# INLINE scanning #-}
