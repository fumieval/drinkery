{-# LANGUAGE Rank2Types, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, LambdaCase, DeriveFunctor, ExistentialQuantification #-}
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
  , wrapTap
  , unTap
  , Trickle(..)
  , consTap
  , orderTap
  , makeTap
  , transTap
  , repeatTap
  , repeatTapM
  , repeatTapM'
  , unfoldrTapM
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
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Semigroup
import Data.Drinkery.Class

-- | @'Tap' m r s@ is a non-monadic, endless producer of @s@. It takes a request
-- @r@.
data Tap r s m = forall t. Tap !t (r -> t -> m (Trickle s t))

data Trickle s t = Trickle s t deriving Functor

mapTrickle :: (a -> b) -> Trickle a t -> Trickle b t
mapTrickle f (Trickle a t) = Trickle (f a) t

wrapTap :: Functor m => (r -> m (s, Tap r s m)) -> Tap r s m
wrapTap f = Tap Nothing $ \r -> \case
  Nothing -> (\(s, t) -> Trickle s (Just t)) <$> f r
  Just (Tap t k) -> fmap (\t' -> Just (Tap t' k)) <$> k r t

unTap :: Monad m => Tap r s m -> r -> m (s, Tap r s m)
unTap (Tap t0 k) r = k r t0 >>= \case
  Trickle s t' -> pure (s, Tap t' k)

data ConsState r t = ConsInitial
    | ConsPending !r
    | ConsCont !t

-- | Prepend a new element, delaying requests.
consTap :: (Semigroup r, Applicative m) => s -> Tap r s m -> Tap r s m
consTap s (Tap t0 k) = Tap ConsInitial $ \r -> \case
  ConsInitial -> pure $ Trickle s (ConsPending r)
  ConsPending r0 -> fmap ConsCont <$> k (r <> r0) t0
  ConsCont t -> fmap ConsCont <$> k r t
{-# INLINE consTap #-}

-- | Send a request to a 'Tap'.
orderTap :: (Semigroup r) => r -> Tap r s m -> Tap r s m
orderTap r (Tap t k) = Tap t $ \r' -> k $! r <> r'
{-# INLINE orderTap #-}

transTap :: Functor n => (forall x. m x -> n x) -> Tap r s m -> Tap r s n
transTap t = go where
  go (Tap f) = Tap $ \r -> fmap go <$> t (f r)
{-# INLINE transTap #-}

-- | Involve an action.
makeTap :: (Monad m) => m (Tap r s m) -> Tap r s m
makeTap m = wrapTap $ \r -> m >>= \t -> unTap t r
{-# INLINE makeTap #-}

repeatTap :: Applicative m => s -> Tap r s m
repeatTap s = Tap () $ \_ _ -> pure (Trickle s ())
{-# INLINE repeatTap #-}

repeatTapM :: Applicative m => m s -> Tap r s m
repeatTapM m = Tap () $ \_ _ -> (\s -> Trickle s ()) <$> m
{-# INLINE repeatTapM #-}

repeatTapM' :: Applicative m => m s -> Tap () s m
repeatTapM' = repeatTapM
{-# INLINE repeatTapM' #-}

unfoldrTapM :: Applicative m => (r -> s -> m (a, s)) -> s -> Tap r a m
unfoldrTapM f = go where
  go s = Tap $ \r -> fmap go <$> f r s

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
  fmap f (Joint (Tap t0 k)) = Joint $ Tap t0 $ \r t -> mapTrickle f <$> k r t
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Joint r m) where
  pure = Joint . repeatTap
  {-# INLINE pure #-}
  Joint (Tap fs0 fu) <*> Joint (Tap xs0 xu) = Joint $ Tap (fs0, xs0)
    $ \r (fs, xs) -> (\(Trickle f fs') (Trickle x xs') -> Trickle (f x) (fs', xs'))
      <$> fu r fs <*> xu r xs

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
  lift m = Producer $ \k -> wrapTap $ \rs -> m >>= \a -> unTap (k a) rs

instance MonadIO m => MonadIO (Producer r s m) where
  liftIO m = Producer $ \k -> wrapTap $ \rs -> liftIO m >>= \a -> unTap (k a) rs

instance MonadSink t m => MonadSink t (Producer p q m) where
  receiving f = lift (receiving f)

instance MonadReader r m => MonadReader r (Producer p q m) where
  ask = lift ask
  local f (Producer m) = Producer $ \k -> transTap (local f) (m k)

instance MonadState s m => MonadState s (Producer p q m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | Produce one element. Orders are put off.
produce :: (Semigroup r, Applicative m) => s -> Producer r s m ()
produce s = Producer $ \cont -> consTap s (cont ())

-- | Accept orders and clear the queue.
accept :: (Monad m, Monoid r) => Producer r s m r
accept = Producer $ \cont -> wrapTap $ \rs -> unTap (cont rs) mempty

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
  lift m = ListT $ \c e -> wrapTap $ \rs -> m >>= \a -> unTap (c a e) rs

instance MonadIO m => MonadIO (ListT r m) where
  liftIO m = ListT $ \c e -> wrapTap $ \rs -> liftIO m >>= \a -> unTap (c a e) rs

instance MonadSink t m => MonadSink t (ListT p m) where
  receiving f = lift (receiving f)

instance MonadReader r m => MonadReader r (ListT p m) where
  ask = lift ask
  local f (ListT m) = ListT $ \c e -> transTap (local f) (m c e)

instance MonadState s m => MonadState s (ListT p m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | Take all the elements in a 'Foldable' container.
sample :: Foldable f => f s -> ListT r m s
sample xs = ListT $ \c e -> foldr c e xs

-- | Get a request.
inquire :: (Monad m, Monoid r) => ListT r m r
inquire = ListT $ \c e -> wrapTap $ \rs -> unTap (c rs e) mempty

-- | End of stream
eof :: (Applicative m, Alternative f) => Tap r (f a) m
eof = repeatTap empty

-- | Run a 'Producer' action and terminate the stream with 'eof'.
tapProducer :: (Monoid r, Applicative m) => Producer r (Maybe s) m a -> Tap r (Maybe s) m
tapProducer m = unProducer m (const eof)
{-# INLINE tapProducer #-}

-- | Specialised 'runProducer'
tapProducer' :: (Applicative m) => Producer () (Maybe s) m a -> Tap () (Maybe s) m
tapProducer' = tapProducer
{-# INLINE tapProducer' #-}

-- | Run 'ListT' and terminate the stream with 'eof'.
tapListT :: (Semigroup r, Applicative m) => ListT r m s -> Tap r (Maybe s) m
tapListT m = unListT m (consTap . pure) eof
{-# INLINE tapListT #-}

-- | Specialised 'runListT'
tapListT' :: (Applicative m) => ListT () m s -> Tap () (Maybe s) m
tapListT' = tapListT
{-# INLINE tapListT' #-}

retractListT :: Monad m => ListT () m s -> m ()
retractListT (ListT f) = go $ f (const $ consTap True) (repeatTap False) where
  go m = unTap m () >>= \(a, k) -> when a (go k)
{-# INLINE retractListT #-}

yield :: (Semigroup r, Applicative f, Applicative m) => s -> Producer r (f s) m ()
yield = produce . pure
{-# INLINE yield #-}
