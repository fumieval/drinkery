{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Drinkery.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict

class Monad m => MonadDrunk r s m | m -> r s where
  drink :: m s
  spit :: s -> m ()
  call :: r -> m ()

instance MonadDrunk r s m => MonadDrunk r s (Reader.ReaderT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance MonadDrunk r s m => MonadDrunk r s (Lazy.StateT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance MonadDrunk r s m => MonadDrunk r s (Strict.StateT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance (Monoid x, MonadDrunk r s m) => MonadDrunk r s (Lazy.WriterT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance (Monoid x, MonadDrunk r s m) => MonadDrunk r s (Strict.WriterT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance (Monoid y, MonadDrunk r s m) => MonadDrunk r s (Lazy.RWST x y z m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance (Monoid y, MonadDrunk r s m) => MonadDrunk r s (Strict.RWST x y z m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance MonadDrunk r s m => MonadDrunk r s (MaybeT m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call

instance MonadDrunk r s m => MonadDrunk r s (ContT x m) where
  drink = lift drink
  spit = lift . spit
  call = lift . call
