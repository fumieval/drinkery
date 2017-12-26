{-# LANGUAGE Rank2Types, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Tap
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Stream producers
-----------------------------------------------------------------------
module Data.Drinkery.Tap (
  -- * Tap
  Tap(..)
  , consTap
  , orderTap
  , makeTap
  , closeTap
  -- * Barman
  , Barman(..)
  , yield
  , accept
  , inexhaustible
  -- * Sommelier
  , Sommelier(..)
  , taste
  , inquire
  -- * Drinker
  , await
  , leftover
  , request
  , smell
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Drinkery.Class

-- | @'Tap' m r s@ is a non-monadic, endless producer of @s@. It takes a request
-- @r@.
newtype Tap r s m = Tap { unTap :: r -> m (s, Tap r s m) }

-- | Prepend a new element, delaying requests.
consTap :: (Monoid r, Applicative m) => s -> Tap r s m -> Tap r s m
consTap s t = Tap $ \r -> pure (s, Tap $ unTap t . mappend r)
{-# INLINE consTap #-}

-- | Send a request to a 'Tap'.
orderTap :: (Monoid r) => r -> Tap r s m -> Tap r s m
orderTap r t = Tap $ \r' -> unTap t $! mappend r r'
{-# INLINE orderTap #-}

-- | Involve an action.
makeTap :: (Monoid r, Monad m) => m (Tap r s m) -> Tap r s m
makeTap m = Tap $ \r -> m >>= \t -> unTap t r
{-# INLINE makeTap #-}

closeTap :: (CloseRequest r, Functor m) => Tap r s m -> m ()
closeTap t = void $ unTap t closeRequest

await :: (Monoid r, MonadDrunk (Tap r s) m) => m s
await = drink $ \t -> unTap t mempty
{-# INLINE await #-}

leftover :: (Monoid r, MonadDrunk (Tap r s) m) => s -> m ()
leftover s = drink $ \t -> return ((), consTap s t)
{-# INLINE leftover #-}

request :: (Monoid r, MonadDrunk (Tap r s) m) => r -> m ()
request r = drink $ \t -> return ((), orderTap r t)
{-# INLINE request #-}

-- | Get one element without consuming.
smell :: (Monoid r, MonadDrunk (Tap r s) m) => m s
smell = do
  s <- await
  leftover s
  return s

-- | Monadic producer
newtype Barman r s m a = Barman { unBarman :: (a -> Tap r s m) -> Tap r s m }

instance Functor (Barman r s m) where
  fmap = liftM

instance Applicative (Barman r s m) where
  pure = return
  (<*>) = ap

instance Monad (Barman r s m) where
  return a = Barman ($ a)
  Barman m >>= k = Barman $ \cont -> m $ \a -> unBarman (k a) cont

instance MonadTrans (Barman r s) where
  lift m = Barman $ \k -> Tap $ \rs -> m >>= \a -> unTap (k a) rs

instance MonadDrunk t m => MonadDrunk t (Barman p q m) where
  drink f = lift (drink f)

-- | Produce one element. Orders are put off.
yield :: (Monoid r, Applicative m) => s -> Barman r s m ()
yield s = Barman $ \cont -> consTap s (cont ())

-- | Accept orders and clear the queue.
accept :: Monoid r => Barman r s m r
accept = Barman $ \cont -> Tap $ \rs -> unTap (cont rs) mempty

-- | Create a infinite 'Tap' from a 'Barman'.
--
-- @inexhaustible :: 'Barman' r s ('Drinker' tap m) x -> 'Distiller' tap m r s@
--
inexhaustible :: Barman r s m x -> Tap r s m
inexhaustible t = unBarman t $ const $ inexhaustible t

-- | Backtracking producer a.k.a. "ListT done right".
newtype Sommelier r m s = Sommelier
  { unSommelier :: forall x. (s -> Tap r x m -> Tap r x m) -> Tap r x m -> Tap r x m }

instance Functor (Sommelier r m) where
  fmap f m = Sommelier $ \c e -> unSommelier m (c . f) e

instance Applicative (Sommelier r m) where
  pure = return
  (<*>) = ap

instance Monad (Sommelier r m) where
  return s = Sommelier $ \c e -> c s e
  m >>= k = Sommelier $ \c e -> unSommelier m (\s -> unSommelier (k s) c) e

instance Alternative (Sommelier r m) where
  empty = Sommelier $ \_ e -> e
  a <|> b = Sommelier $ \c e -> unSommelier a c (unSommelier b c e)

instance MonadTrans (Sommelier r) where
  lift m = Sommelier $ \c e -> Tap $ \rs -> m >>= \a -> unTap (c a e) rs

instance MonadIO m => MonadIO (Sommelier r m) where
  liftIO m = Sommelier $ \c e -> Tap $ \rs -> liftIO m >>= \a -> unTap (c a e) rs

instance MonadDrunk t m => MonadDrunk t (Sommelier p m) where
  drink f = lift (drink f)

-- | Take all the elements in a 'Foldable' container.
taste :: Foldable f => f s -> Sommelier r m s
taste xs = Sommelier $ \c e -> foldr c e xs

-- | Get a request.
inquire :: Monoid r => Sommelier r m r
inquire = Sommelier $ \c e -> Tap $ \rs -> unTap (c rs e) mempty
