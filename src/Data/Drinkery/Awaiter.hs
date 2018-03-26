{-# LANGUAGE LambdaCase, DeriveFunctor, FlexibleContexts #-}
module Data.Drinkery.Awaiter where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Drinkery.Class
import Data.Drinkery.Tap
import Data.Semigroup

-- | @Awaiter s@ is a simple consumer of @s@. Unlike 'Sink', it can be
-- partially run.
--
-- 'serving' distributes each input to a list of 'Awaiter's until all the patrons
-- terminate.
-- ('<|>') returns the first result.
--
newtype Awaiter s m a = Awaiter { runAwaiter :: m (Either (s -> Awaiter s m a) a) }

instance Functor m => Functor (Awaiter s m) where
  fmap f (Awaiter m) = Awaiter $ either (Left . (fmap f .)) (Right . f) <$> m

instance Monad m => Applicative (Awaiter s m) where
  pure a = Awaiter $ pure $ Right a
  {-# INLINE pure #-}
  m <*> k = Awaiter $ runAwaiter m >>= \case
    Right f -> runAwaiter $ f <$> k
    Left f -> pure $ Left $ (<*> k) . f

instance Monad m => Monad (Awaiter s m) where
  return = pure
  Awaiter m >>= k = Awaiter $ m >>= \case
    Right a -> runAwaiter (k a)
    Left f -> pure $ Left $ (>>=k) . f

instance MonadTrans (Awaiter s) where
  lift = Awaiter . fmap Right

instance Monad m => Alternative (Awaiter s m) where
  empty = Awaiter $ pure $ Left $ const empty
  Awaiter l <|> Awaiter r = Awaiter $ l >>= \case
    Left f -> r >>= \case
      Left g -> return $ Left $ \x -> f x <|> g x
      Right a -> return $ Right a
    Right a -> return $ Right a

instance Monad m => MonadPlus (Awaiter s m) where
  mzero = empty
  mplus = (<|>)

instance MonadIO m => MonadIO (Awaiter s m) where
  liftIO = Awaiter . fmap Right . liftIO

await :: Monad m => Awaiter s m s
await = Awaiter $ pure $ Left pure
{-# INLINE await #-}

serving_ :: Monad m => [Awaiter s m a] -> Awaiter s m ()
serving_ t0 = lift (gather runAwaiter t0) >>= go
  where
    gather k = loop where
      loop (m : ms) = k m >>= \case
        Left f -> (f :) <$> loop ms
        Right _ -> loop ms
      loop [] = pure []
    go [] = return ()
    go t = do
      s <- await
      lift (gather (\f -> runAwaiter (f s)) t) >>= go

iterAwaiter :: Monad m => m s -> Awaiter s m a -> m a
iterAwaiter k = go where
  go m = runAwaiter m >>= \case
    Left f -> k >>= go . f
    Right a -> return a
{-# INLINE iterAwaiter #-}

-- | @iterAwaiterT consume :: Awaiter s m a -> Sink s m a@
iterAwaiterT :: (Monad m, MonadTrans t, Monad (t m)) => t m s -> Awaiter s m a -> t m a
iterAwaiterT k = go where
  go m = lift (runAwaiter m) >>= \case
    Left f -> k >>= go . f
    Right a -> return a
{-# INLINE iterAwaiterT #-}

lookAheadT :: (Monad m, MonadTrans t, Monoid r, Semigroup r, MonadSink (Tap r s) (t m)) => Awaiter s m a -> t m a
lookAheadT = go [] where
  go xs m = lift (runAwaiter m) >>= \case
    Right a -> a <$ mapM_ leftover (reverse xs)
    Left f -> consume >>= \s -> go (s : xs) (f s)
