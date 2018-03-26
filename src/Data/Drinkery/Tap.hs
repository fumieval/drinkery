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
  , repeatTap
  , repeatTapM
  , repeatTapM'
  , Joint(..)
  -- * Producer
  , Producer(..)
  , yield
  , accept
  , inexhaustible
  , tapProducer
  , tapProducer'
  , produce
  -- * ListT
  , ListT(..)
  , sample
  , inquire
  , tapListT
  , tapListT'
  , retractListT
  -- * Sink
  , consume
  , leftover
  , request
  , prefetch
  -- * End of stream
  , eof
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Drinkery.Class

-- | @'Tap' m r s@ is a non-monadic, endless producer of @s@. It takes a request
-- @r@.
newtype Tap r s m = Tap { unTap :: r -> m (s, Tap r s m) }

-- | Prepend a new element, delaying requests.
consTap :: (Semigroup r, Applicative m) => s -> Tap r s m -> Tap r s m
consTap s t = Tap $ \r -> pure (s, Tap $ unTap t . (<>) r)
{-# INLINE consTap #-}

-- | Send a request to a 'Tap'.
orderTap :: (Semigroup r) => r -> Tap r s m -> Tap r s m
orderTap r t = Tap $ \r' -> unTap t $! r <> r'
{-# INLINE orderTap #-}

-- | Involve an action.
makeTap :: (Monad m) => m (Tap r s m) -> Tap r s m
makeTap m = Tap $ \r -> m >>= \t -> unTap t r
{-# INLINE makeTap #-}

repeatTap :: Applicative m => s -> Tap r s m
repeatTap s = go where
  go = Tap $ const $ pure (s, go)
{-# INLINE repeatTap #-}

repeatTapM :: Applicative m => m s -> Tap r s m
repeatTapM m = go where
  go = Tap $ const $ flip (,) go <$> m
{-# INLINE repeatTapM #-}

repeatTapM' :: Applicative m => m s -> Tap () s m
repeatTapM' = repeatTapM
{-# INLINE repeatTapM' #-}

instance CloseRequest r => Closable (Tap r s) where
  close t = void $ unTap t closeRequest

consume :: (Monoid r, MonadSink (Tap r s) m) => m s
consume = receiving $ \t -> unTap t mempty
{-# INLINE consume #-}

leftover :: (Semigroup r, MonadSink (Tap r s) m) => s -> m ()
leftover s = receiving $ \t -> return ((), consTap s t)
{-# INLINE leftover #-}

request :: (Semigroup r, MonadSink (Tap r s) m) => r -> m ()
request r = receiving $ \t -> return ((), orderTap r t)
{-# INLINE request #-}

-- | Get one element without consuming.
prefetch :: (Monoid r, Semigroup r, MonadSink (Tap r s) m) => m s
prefetch = do
  s <- consume
  leftover s
  return s
{-# INLINE prefetch #-}

-- | ('<*>') zips two taps.
newtype Joint r m s = Joint { unJoint :: Tap r s m }

instance Functor m => Functor (Joint r m) where
  fmap f (Joint tap0) = Joint (go tap0) where
    go tap = Tap $ \r -> fmap (\(s, t) -> (f s, go t)) $ unTap tap r
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Joint r m) where
  pure = Joint . repeatTap
  {-# INLINE pure #-}
  Joint tapF <*> Joint tapA = Joint (go tapF tapA) where
    go s t = Tap $ \r -> (\(f, s') (x, t') -> (f x, go s' t'))
      <$> unTap s r
      <*> unTap t r
  {-# INLINE (<*>) #-}

-- | Monadic producer
newtype Producer r s m a = Producer { unProducer :: (a -> Tap r s m) -> Tap r s m }

instance Functor (Producer r s m) where
  fmap f (Producer m) = Producer $ \cont -> m (cont . f)

instance Applicative (Producer r s m) where
  pure = return
  Producer m <*> Producer k = Producer $ \cont -> m $ \f -> k $ cont . f

instance Monad (Producer r s m) where
  return a = Producer ($ a)
  Producer m >>= k = Producer $ \cont -> m $ \a -> unProducer (k a) cont

instance MonadTrans (Producer r s) where
  lift m = Producer $ \k -> Tap $ \rs -> m >>= \a -> unTap (k a) rs

instance MonadIO m => MonadIO (Producer r s m) where
  liftIO m = Producer $ \k -> Tap $ \rs -> liftIO m >>= \a -> unTap (k a) rs

instance MonadSink t m => MonadSink t (Producer p q m) where
  receiving f = lift (receiving f)

-- | Produce one element. Orders are put off.
produce :: (Semigroup r, Applicative m) => s -> Producer r s m ()
produce s = Producer $ \cont -> consTap s (cont ())

-- | Accept orders and clear the queue.
accept :: Monoid r => Producer r s m r
accept = Producer $ \cont -> Tap $ \rs -> unTap (cont rs) mempty

-- | Create a infinite 'Tap' from a 'Producer'.
--
-- @inexhaustible :: 'Producer' r s ('Sink' tap m) x -> 'Distiller' tap m r s@
--
inexhaustible :: Producer r s m x -> Tap r s m
inexhaustible t = go where
  go = unProducer t $ const go
{-# INLINE inexhaustible #-}

-- | Backtracking producer a.k.a. "ListT done right".
newtype ListT r m s = ListT
  { unListT :: forall x. (s -> Tap r x m -> Tap r x m) -> Tap r x m -> Tap r x m }

instance Functor (ListT r m) where
  fmap f m = ListT $ \c e -> unListT m (c . f) e

instance Applicative (ListT r m) where
  pure = return
  (<*>) = ap

instance Monad (ListT r m) where
  return s = ListT $ \c e -> c s e
  m >>= k = ListT $ \c e -> unListT m (\s -> unListT (k s) c) e

instance Alternative (ListT r m) where
  empty = ListT $ \_ e -> e
  a <|> b = ListT $ \c e -> unListT a c (unListT b c e)

instance MonadPlus (ListT r m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (ListT r) where
  lift m = ListT $ \c e -> Tap $ \rs -> m >>= \a -> unTap (c a e) rs

instance MonadIO m => MonadIO (ListT r m) where
  liftIO m = ListT $ \c e -> Tap $ \rs -> liftIO m >>= \a -> unTap (c a e) rs

instance MonadSink t m => MonadSink t (ListT p m) where
  receiving f = lift (receiving f)

-- | Take all the elements in a 'Foldable' container.
sample :: Foldable f => f s -> ListT r m s
sample xs = ListT $ \c e -> foldr c e xs

-- | Get a request.
inquire :: Monoid r => ListT r m r
inquire = ListT $ \c e -> Tap $ \rs -> unTap (c rs e) mempty

-- | End of stream
eof :: (Applicative m, Alternative f) => Tap r (f a) m
eof = repeatTap empty

-- | Run a 'Producer' action and terminate the stream with 'eof'.
tapProducer :: (Monoid r, Applicative m, Alternative f) => Producer r (f s) m a -> Tap r (f s) m
tapProducer m = unProducer m (const eof)
{-# INLINE tapProducer #-}

-- | Specialised 'runProducer'
tapProducer' :: (Applicative m, Alternative f) => Producer () (f s) m a -> Tap () (f s) m
tapProducer' = tapProducer
{-# INLINE tapProducer' #-}

-- | Run 'ListT' and terminate the stream with 'eof'.
tapListT :: (Semigroup r, Applicative m, Alternative f) => ListT r m s -> Tap r (f s) m
tapListT m = unListT m (consTap . pure) eof
{-# INLINE tapListT #-}

-- | Specialised 'runListT'
tapListT' :: (Applicative m, Alternative f) => ListT () m s -> Tap () (f s) m
tapListT' = tapListT
{-# INLINE tapListT' #-}

retractListT :: Monad m => ListT () m s -> m ()
retractListT (ListT f) = go $ f (const $ consTap True) (repeatTap False) where
  go m = unTap m () >>= \(a, k) -> when a (go k)
{-# INLINE retractListT #-}

yield :: (Semigroup r, Applicative f, Applicative m) => s -> Producer r (f s) m ()
yield = produce . pure
{-# INLINE yield #-}
