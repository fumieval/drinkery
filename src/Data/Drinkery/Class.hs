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
  ( Sink(..)
  , mapSink
  , runSink
  , MonadSink(..)
  , CloseRequest(..)
  , Closable(..)) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont hiding (cont)
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

-- | A 'Sink' is a stream consumer monad.
newtype Sink t m a = Sink
  { unSink :: forall r. t m -> (a -> t m -> m r) -> m r }

mapSink :: (forall x. m x -> m x) -> Sink t m a -> Sink t m a
mapSink t (Sink d) = Sink $ \tap k -> t (d tap k)

runSink :: Applicative m => Sink t m a -> t m -> m (a, t m)
runSink (Sink d) t = d t (\a t' -> pure (a, t'))

instance Functor (Sink s m) where
  fmap f m = Sink $ \s k -> unSink m s (k . f)

instance Applicative (Sink s m) where
  pure a = Sink $ \s k -> k a s
  Sink mf <*> Sink mx = Sink
    $ \s k -> mf s $ \f s' -> mx s' $ k . f
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance Monad (Sink s m) where
  return = pure
  {-# INLINE return #-}
  m >>= k = Sink $ \s cont -> unSink m s $ \a s' -> unSink (k a) s' cont

instance MonadTrans (Sink t) where
  lift m = Sink $ \t k -> m >>= \a -> k a t

instance MonadIO m => MonadIO (Sink t m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (Sink t m) where
  ask = lift ask
  local f = mapSink (local f)

instance MonadState s m => MonadState s (Sink t m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter s m => MonadWriter s (Sink t m) where
  writer = lift . writer
  tell   = lift . tell
  listen m = Sink $ \s k -> do
    ((a, s'), w) <- listen (runSink m s)
    k (a, w) s'
  pass m = Sink $ \s k -> join $ pass $ do
    ((a, f), s') <- runSink m s
    return (k a s', f)

-- | Monads that drink
class Monad m => MonadSink t m | m -> t where
  receiving :: (forall n. Monad n => t n -> n (a, t n)) -> m a

instance Monad m => MonadSink t (Sink t m) where
  receiving f = Sink $ \t k -> f t >>= uncurry k
  {-# INLINE receiving #-}

instance MonadSink t m => MonadSink t (Reader.ReaderT x m) where
  receiving f = lift (receiving f)

instance MonadSink t m => MonadSink t (Lazy.StateT x m) where
  receiving f = lift (receiving f)

instance MonadSink t m => MonadSink t (Strict.StateT x m) where
  receiving f = lift (receiving f)

instance (Monoid x, MonadSink t m) => MonadSink t (Lazy.WriterT x m) where
  receiving f = lift (receiving f)

instance (Monoid x, MonadSink t m) => MonadSink t (Strict.WriterT x m) where
  receiving f = lift (receiving f)

instance (Monoid y, MonadSink t m) => MonadSink t (Lazy.RWST x y z m) where
  receiving f = lift (receiving f)

instance (Monoid y, MonadSink t m) => MonadSink t (Strict.RWST x y z m) where
  receiving f = lift (receiving f)

instance MonadSink t m => MonadSink t (MaybeT m) where
  receiving f = lift (receiving f)

instance MonadSink t m => MonadSink t (ContT x m) where
  receiving f = lift (receiving f)

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
