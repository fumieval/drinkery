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
  , mapping
  , traversing
  , filtering
  , scanning
  ) where

import Control.Monad.Trans
import Data.Drinkery.Tap
import Data.Drinkery.Class
import Data.Tuple

-- | @Distiller tap m r s@ is a stream transducer which has four parameters:
--
-- * @tap@ input
-- * @r@ request from the downstream
-- * @s@ output
-- * @m@ underlying monad
--
-- This is also a 'Tap'.
--
type Distiller tap r s m = Tap r s (Drinker tap m)

infix 6 +&
infixr 7 $&
infixr 7 ++&
infixl 8 ++$

-- | Connect a tap with a Drinker. Flipped 'runDrinker'.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap (along with the result).
-- * @&@ Right operand is a Drinker.
(++&) :: (Functor m) => tap m -> Drinker tap m a -> m (tap m, a)
d ++& b = swap <$> runDrinker b d
{-# INLINE (++&) #-}

-- | Attach a distiller to a tap.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap.
-- * @$@ Right operand is a distiller.
--
(++$) :: (Functor m) => tap m -> Distiller tap r s m -> Tap r s m
(++$) = go where -- looks strange, but seems to perform better (GHC 8.2.2)
  go t d = Tap $ \r -> (\((s, d'), t') -> (s, go t' d'))
    <$> runDrinker (unTap d r) t
{-# INLINE (++$) #-}

-- | Feed a tap to a drinker and close the used tap.
(+&) :: (Closable tap, Monad m) => tap m -> Drinker tap m a -> m a
t +& b = do
  (a, t') <- runDrinker b t
  close t'
  return a
{-# INLINE (+&) #-}

-- | Like ('+&') but discards the used tap.
--
-- @($&) :: Distiller tap m r s -> Drinker (Tap r s) (Drinker tap m) a -> Drinker tap m a@
--
($&) :: (Monad m) => tap m -> Drinker tap m a -> m a
t $& b = fmap fst $ runDrinker b t
{-# INLINE ($&) #-}

-- | Create a request-preserving distiller.
propagating :: (Monoid r, Monad m) => Drinker (Tap r a) m (b, Distiller (Tap r a) r b m) -> Distiller (Tap r a) r b m
propagating m = Tap $ \r -> request r >> m
{-# INLINE propagating #-}

mapping :: (Monoid r, Monad m) => (a -> b) -> Distiller (Tap r a) r b m
mapping f = go where
  go = propagating $ drink >>= \a -> return (f a, go)
{-# INLINE mapping #-}

traversing :: (Monoid r, Monad m) => (a -> m b) -> Distiller (Tap r a) r b m
traversing f = go where
  go = propagating $ drink >>= \a -> lift (f a) >>= \b -> return (b, go)

filtering :: (Monoid r, Monad m) => (a -> Bool) -> Distiller (Tap r a) r a m
filtering f = go where
  go = propagating $ drink >>= \a -> if f a
    then return (a, go)
    else unTap go mempty

scanning :: (Monoid r, Monad m) => (b -> a -> b) -> b -> Distiller (Tap r a) r b m
scanning f b0 = consTap b0 $ go b0 where
  go b = propagating $ fmap (\a -> let !b' = f b a in (b', go $ b')) drink
{-# INLINE scanning #-}
