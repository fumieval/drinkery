{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Data.Drinkery.Patron where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- | @Patron s@ is a simple consumer of @s@. Unlike 'Drinker', it can be
-- partially run.
--
-- 'serving' distributes each input to a list of 'Patron's until all the patrons
-- terminate.
-- ('<|>') returns the first result.
--
newtype Patron s m a = Patron { runPatron :: m (Either (s -> Patron s m a) a) }

instance Functor m => Functor (Patron s m) where
  fmap f (Patron m) = Patron $ either (Left . (fmap f .)) (Right . f) <$> m

instance Monad m => Applicative (Patron s m) where
  pure a = Patron $ pure $ Right a
  {-# INLINE pure #-}
  m <*> k = Patron $ runPatron m >>= \case
    Right f -> runPatron $ f <$> k
    Left f -> pure $ Left $ (<*> k) . f

instance Monad m => Monad (Patron s m) where
  return = pure
  Patron m >>= k = Patron $ m >>= \case
    Right a -> runPatron (k a)
    Left f -> pure $ Left $ (>>=k) . f

instance MonadTrans (Patron s) where
  lift = Patron . fmap Right

instance Monad m => Alternative (Patron s m) where
  empty = Patron $ pure $ Left $ const empty
  Patron l <|> Patron r = Patron $ l >>= \case
    Left f -> r >>= \case
      Left g -> return $ Left $ \x -> f x <|> g x
      Right a -> return $ Right a
    Right a -> return $ Right a

instance Monad m => MonadPlus (Patron s m) where
  mzero = empty
  mplus = (<|>)

await :: Monad m => Patron s m s
await = Patron $ pure $ Left pure
{-# INLINE await #-}

serving_ :: Monad m => [Patron s m a] -> Patron s m ()
serving_ t0 = lift (gather runPatron t0) >>= go
  where
    gather k = loop where
      loop (m : ms) = k m >>= \case
        Left f -> (f :) <$> loop ms
        Right _ -> loop ms
      loop [] = pure []
    go [] = return ()
    go t = do
      s <- await
      lift (gather (\f -> runPatron (f s)) t) >>= go

iterPatron :: Monad m => m s -> Patron s m a -> m a
iterPatron k = go where
  go m = runPatron m >>= \case
    Left f -> k >>= go . f
    Right a -> return a
{-# INLINE iterPatron #-}

-- | @iterPatronT drink :: Patron s m a -> Drinker s m a@
iterPatronT :: (Monad m, MonadTrans t, Monad (t m)) => t m s -> Patron s m a -> t m a
iterPatronT k = go where
  go m = lift (runPatron m) >>= \case
    Left f -> k >>= go . f
    Right a -> return a
{-# INLINE iterPatronT #-}
