{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Data.Drinkery.Tap where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Drinkery.Boozer
import Data.Drinkery.Class

-- | Non-monadic, endless producer
newtype Tap m r s = Tap { unTap :: r -> m (s, Tap m r s) }

-- | Prepend a new element, delaying requests.
consTap :: (Monoid r, Applicative m) => s -> Tap m r s -> Tap m r s
consTap s r = Tap $ extend (\w -> pure (s, Tap w)) (unTap r)
{-# INLINE consTap #-}

orderTap :: (Monoid r) => r -> Tap m r s -> Tap m r s
orderTap r t = Tap $ \r' -> unTap t $! mappend r r'

-- | Monadic producer
newtype Barman r s m a = Barman { unBarman :: (a -> Tap m r s) -> Tap m r s }

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

instance MonadDrunk r s m => MonadDrunk r s (Barman p q m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

-- | Produce one element. Orders are put off.
topup :: (Monoid r, Applicative m) => s -> Barman r s m ()
topup s = Barman $ \cont -> consTap s (cont ())

-- | Accept orders and clear the queue.
accept :: Monoid r => Barman r s m r
accept = Barman $ \cont -> Tap $ \rs -> unTap (cont rs) mempty

-- | Create a 'Tap' from a 'Barman'.
--
-- @Barman r s (Boozer p q m) x -> Distiller p q m r s@
--
inexhaustible :: Barman r s m x -> Tap m r s
inexhaustible t = unBarman t $ const $ inexhaustible t

-- | Backtracking producer a.k.a. "ListT done right".
newtype Sommelier r m s = Sommelier
  { unSommelier :: forall x. (s -> Tap m r x -> Tap m r x) -> Tap m r x -> Tap m r x }

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

taste :: Foldable f => f s -> Sommelier r m s
taste xs = Sommelier $ \c e -> foldr c e xs

inquire :: Monoid r => Sommelier r m r
inquire = Sommelier $ \c e -> Tap $ \rs -> unTap (c rs e) mempty
