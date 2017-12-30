{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Class
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Basic classes
-----------------------------------------------------------------------
module Data.Drinkery.Class
  ( Drinker(..)
  , mapDrinker
  , MonadDrunk(..)
  , CloseRequest(..)
  , Closable(..)) where

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
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class

-- | A 'Drinker' is a stream consumer monad.
newtype Drinker t m a = Drinker { runDrinker :: t m -> m (a, t m) }

mapDrinker :: (forall x. m x -> m x) -> Drinker t m a -> Drinker t m a
mapDrinker t = Drinker . fmap t . runDrinker
{-# INLINE mapDrinker #-}

instance (Functor m) => Functor (Drinker s m) where
  fmap f m = Drinker $ \s -> fmap (\(a, s') -> (f a, s')) $ runDrinker m s
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (Drinker s m) where
  pure a = Drinker $ \s -> return (a, s)
  {-# INLINE pure #-}
  Drinker mf <*> Drinker mx = Drinker $ \ s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')
  {-# INLINE (<*>) #-}
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance (Monad m) => Monad (Drinker s m) where
  return a = Drinker $ \s -> return (a, s)
  {-# INLINE return #-}
  m >>= k  = Drinker $ \s -> do
    (a, s') <- runDrinker m s
    runDrinker (k a) s'
  {-# INLINE (>>=) #-}
  fail str = Drinker $ \_ -> fail str
  {-# INLINE fail #-}

instance MonadTrans (Drinker t) where
  lift m = Drinker $ \t -> m >>= \a -> return (a, t)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Drinker t m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (Drinker t m) where
  ask = lift ask
  local f = mapDrinker (local f)

instance MonadState s m => MonadState s (Drinker t m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter s m => MonadWriter s (Drinker t m) where
  writer = lift . writer
  tell   = lift . tell
  listen m = Drinker $ \s -> do
    ((a, s'), w) <- listen (runDrinker m s)
    return ((a, w), s')
  pass m = Drinker $ \s -> pass $ do
    ((a, f), s') <- runDrinker m s
    return ((a, s'), f)

-- | Monads that drink
class Monad m => MonadDrunk t m | m -> t where
  drinking :: (forall n. Monad n => t n -> n (a, t n)) -> m a

instance Monad m => MonadDrunk t (Drinker t m) where
  drinking = Drinker

instance MonadDrunk t m => MonadDrunk t (Reader.ReaderT x m) where
  drinking f = lift (drinking f)

instance MonadDrunk t m => MonadDrunk t (Lazy.StateT x m) where
  drinking f = lift (drinking f)

instance MonadDrunk t m => MonadDrunk t (Strict.StateT x m) where
  drinking f = lift (drinking f)

instance (Monoid x, MonadDrunk t m) => MonadDrunk t (Lazy.WriterT x m) where
  drinking f = lift (drinking f)

instance (Monoid x, MonadDrunk t m) => MonadDrunk t (Strict.WriterT x m) where
  drinking f = lift (drinking f)

instance (Monoid y, MonadDrunk t m) => MonadDrunk t (Lazy.RWST x y z m) where
  drinking f = lift (drinking f)

instance (Monoid y, MonadDrunk t m) => MonadDrunk t (Strict.RWST x y z m) where
  drinking f = lift (drinking f)

instance MonadDrunk t m => MonadDrunk t (MaybeT m) where
  drinking f = lift (drinking f)

instance MonadDrunk t m => MonadDrunk t (ContT x m) where
  drinking f = lift (drinking f)

class CloseRequest a where
  -- | A value representing a close request
  closeRequest :: a

instance CloseRequest () where
  closeRequest = ()

instance CloseRequest a => CloseRequest [a] where
  closeRequest = [closeRequest]

-- | Closable tap
class Closable t where
  close :: Monad m => t m -> m ()
