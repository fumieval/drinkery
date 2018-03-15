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
  , reservingTap
  , echo
  , mapping
  , traversing
  , filtering
  , scanning
  , repeating
  ) where

import Control.Monad.Catch (onException, MonadCatch)
import Control.Monad.Trans
import Data.Drinkery.Tap
import Data.Drinkery.Class
import Data.Semigroup
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
(+&) :: (Closable tap, MonadCatch m) => tap m -> Drinker tap m a -> m a
t +& b = do
  (a, t') <- runDrinker b t `onException` close t
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

echo :: Monad m => Distiller (Tap r s) r s m
echo = mapping id

mapping :: (Monad m) => (a -> b) -> Distiller (Tap r a) r b m
mapping f = go where
  go = Tap $ \r -> drinking $ \t -> fmap (\(s, t') -> ((f s, go), t')) $ unTap t r
{-# INLINE mapping #-}

-- | Get one element preserving a request
reservingTap :: MonadDrunk (Tap r a) m => (a -> m (s, Tap r s m)) -> Tap r s m
reservingTap k = Tap $ \r -> do
  a <- drinking $ \t -> unTap t r
  k a

traversing :: (Monad m) => (a -> m b) -> Distiller (Tap r a) r b m
traversing f = go where
  go = reservingTap $ \a -> do
    b <- lift $ f a
    return (b, go)

filtering :: (Monoid r, Monad m) => (a -> Bool) -> Distiller (Tap r a) r a m
filtering f = go where
  go = reservingTap $ \a -> if f a
    then return (a, go)
    else unTap go mempty

scanning :: Monad m => (b -> a -> b) -> b -> Distiller (Tap r a) r b m
scanning f b0 = go b0 where
  go b = reservingTap $ \a -> do
    let !b' = f b a
    return (b', go $ b')
{-# INLINE scanning #-}

-- | Create a request-preserving distiller from a drinker action.
repeating :: (MonadDrunk (Tap r a) m, Semigroup r) => m b -> Tap r b m
repeating m = go where
  go = Tap $ \r -> do
    request r
    a <- m
    return (a, go)
