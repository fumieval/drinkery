{-# LANGUAGE Rank2Types, BangPatterns, LambdaCase, FlexibleContexts #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Sinky.Distiller
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

-- | @Distiller tap m r s@ is a stream transducer which has four parameters:
--
-- * @tap@ input
-- * @r@ request from the downstream
-- * @s@ output
-- * @m@ underlying monad
--
-- This is also a 'Tap'.
--
type Distiller tap r s m = Tap r s (Sink tap m)

infix 6 +&
infixr 7 $&
infixr 7 ++&
infixl 8 ++$

-- | Connect a tap with a Sink. Flipped 'runSink'.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap (along with the result).
-- * @&@ Right operand is a Sink.
(++&) :: (Applicative m) => tap m -> Sink tap m a -> m (tap m, a)
d ++& b = unSink b d $ \a t -> pure (t, a)
{-# INLINE (++&) #-}

-- | Attach a distiller to a tap.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap.
-- * @$@ Right operand is a distiller.
--
(++$) :: (Applicative m) => tap m -> Distiller tap r s m -> Tap r s m
t0 ++$ Tap ds0 u = Tap (t0, ds0) $ \r (t, ds) -> unSink (u r ds) t
  $ \r t' -> pure $ (,) t' <$> r
{-# INLINE (++$) #-}

-- | Feed a tap to a drinker and close the used tap.
(+&) :: (Closable tap, MonadCatch m) => tap m -> Sink tap m a -> m a
t +& b = do
  (a, t') <- runSink b t `onException` close t
  close t'
  return a
{-# INLINE (+&) #-}

-- | Like ('+&') but discards the used tap.
--
-- @($&) :: Distiller tap m r s -> Sink (Tap r s) (Sink tap m) a -> Sink tap m a@
--
($&) :: (Monad m) => tap m -> Sink tap m a -> m a
t $& b = fmap fst $ runSink b t
{-# INLINE ($&) #-}

echo :: Monad m => Distiller (Tap r s) r s m
echo = mapping id
{-# INLINE echo #-}

mapping :: (Monad m) => (a -> b) -> Distiller (Tap r a) r b m
mapping f = Tap () $ \r _ -> Sink $ \(Tap t k) cont -> do
  Trickle a t' <- k r t
  cont (Trickle (f a) ()) (Tap t' k)
{-# INLINE mapping #-}

-- | Get one element preserving a request
reservingTap :: Monad m => (a -> Sink (Tap r a) m (b, Distiller (Tap r a) r b m)) -> Distiller (Tap r a) r b m
reservingTap k = wrapTap $ \r -> Sink $ \t cont -> do
  (a, t') <- unTap t r
  unSink (k a) t' cont
{-# INLINE reservingTap #-}

traversing :: (Monad m) => (a -> m b) -> Distiller (Tap r a) r b m
traversing f = Tap () $ \r _ -> Sink $ \(Tap t k) cont -> do
  Trickle a t' <- k r t
  b <- f a
  cont (Trickle b ()) (Tap t' k)
{-# INLINE traversing #-}

filtering :: (Monoid r, Monad m) => (a -> Bool) -> Distiller (Tap r a) r a m
filtering f = go where
  go = reservingTap $ \a -> if f a
    then return (a, go)
    else unTap go mempty
{-# INLINE filtering #-}

scanning :: Monad m => (b -> a -> b) -> b -> Distiller (Tap r a) r b m
scanning f b0 = go b0 where
  go b = reservingTap $ \a -> do
    let !b' = f b a
    return (b', go $ b')
{-# INLINE scanning #-}

-- | Create a request-preserving distiller from a drinker action.
repeating :: (MonadSink (Tap r a) m, Semigroup r) => m b -> Tap r b m
repeating m = go where
  go = wrapTap $ \r -> do
    request r
    a <- m
    return (a, go)
