{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Boozer
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Basic consumer
-----------------------------------------------------------------------
module Data.Drinkery.Boozer where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Drinkery.Class

-- | Boozer is the initial encoding of a consumer.
data Boozer r s m a = Drink (s -> Boozer r s m a)
  | Spit s (Boozer r s m a)
  | Call r (Boozer r s m a)
  | Lift (m (Boozer r s m a))
  | Pure a
  deriving Functor

-- | Tear down a 'Boozer', maintaining a stack of leftovers.
iterBoozer :: ([s] -> a -> z) -- ^ return
  -> ((s -> z) -> z) -- ^ drink
  -> (r -> z -> z) -- ^ call
  -> (forall x. m x -> (x -> z) -> z) -- ^ bind
  -> Boozer r s m a -> z
iterBoozer p d c t = go [] where
  go [] (Drink k) = d (go [] . k)
  go (x : xs) (Drink k) = go xs (k x)
  go xs (Spit s k) = go (s : xs) k
  go xs (Call r k) = c r (go xs k)
  go xs (Lift m) = t m (go xs)
  go xs (Pure a) = p xs a

hoistBoozer :: Functor n => (forall x. m x -> n x) -> Boozer r s m a -> Boozer r s n a
hoistBoozer t = go where
  go (Pure a) = Pure a
  go (Lift m) = Lift $ go <$> t m
  go (Call r k) = Call r (go k)
  go (Spit s k) = Spit s (go k)
  go (Drink f) = Drink $ go . f

instance Functor m => Applicative (Boozer r s m) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = ap
  (*>) = (>>)

instance Functor m => Monad (Boozer r s m) where
  return = Pure
  {-# INLINE return #-}
  m0 >>= k = go m0 where
    go (Pure a) = k a
    go (Drink m) = Drink $ go . m
    go (Lift m) = Lift $ fmap go m
    go (Call r c) = Call r (go c)
    go (Spit s c) = Spit s (go c)

  m0 >> k = go m0 where
    go (Pure _) = k
    go (Drink m) = Drink $ go . m
    go (Lift m) = Lift $ fmap go m
    go (Call r c) = Call r (go c)
    go (Spit s c) = Spit s (go c)

instance MonadTrans (Boozer r s) where
  lift m = Lift $ Pure <$> m

instance MonadIO m => MonadIO (Boozer r s m) where
  liftIO m = Lift $ Pure <$> liftIO m

instance MonadReader x m => MonadReader x (Boozer r s m) where
  ask = lift ask
  local f = hoistBoozer (local f)

instance MonadState x m => MonadState x (Boozer r s m) where
  get = lift get
  put = lift . put
  state = lift . state

instance Functor m => MonadDrunk r s (Boozer r s m) where
  drink = Drink Pure
  spit s = Spit s (Pure ())
  call r = Call r (Pure ())

-- | 'Patron' is a CPS'd 'Boozer'.
newtype Patron r s m a = Patron
  { unPatron :: forall x. (a -> Boozer r s m x) -> Boozer r s m x }

runPatron :: Patron r s m a -> Boozer r s m a
runPatron m = unPatron m Pure
{-# INLINE runPatron #-}

instance Functor (Patron r s m) where
  fmap f m = Patron $ \cont -> unPatron m $ cont . f
  {-# INLINE fmap #-}

instance Applicative (Patron r s m) where
  pure a = Patron $ \cont -> cont a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  (*>) = (>>)
  {-# INLINE (*>) #-}

instance Monad (Patron r s m) where
  return a = Patron ($a)
  Patron m >>= k = Patron $ \cont -> m (\a -> unPatron (k a) cont)

instance MonadTrans (Patron r s) where
  lift m = Patron $ \cont -> Lift $ cont <$> m

instance MonadDrunk r s (Patron r s m) where
  drink = Patron $ \cont -> Drink cont
  spit s = Patron $ \cont -> Spit s $ cont ()
  call r = Patron $ \cont -> Call r $ cont ()

instance MonadReader x m => MonadReader x (Patron r s m) where
  ask = lift ask
  local f m = Patron $ \cont -> unPatron m (local f . cont)

instance MonadState x m => MonadState x (Patron r s m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadIO m => MonadIO (Patron r s m) where
  liftIO m = Patron $ \cont -> Lift $ cont <$> liftIO m
