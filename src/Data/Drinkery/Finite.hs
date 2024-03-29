{-# LANGUAGE BangPatterns, Rank2Types, FlexibleContexts, LambdaCase #-}
module Data.Drinkery.Finite where

import Control.Monad (replicateM_)
import Data.Drinkery.Class
import Data.Drinkery.Distiller
import Data.Drinkery.Tap
import Data.Semigroup

-- | Finite source
type Source r s = Tap r (Maybe s)

-- | Mono in/out
type Converter p q r s m = Source r s (Sink (Source p q) m)

type Pipe a b m = forall r. (Monoid r, Semigroup r) => Converter r a r b m

scan :: Monad m => (b -> a -> b) -> b -> Pipe a b m
scan f b0 = consTap (Just b0) $ go b0 where
  go b = reservingTap $ \case
    Just a -> let !b' = f b a in return (Just b', go b')
    Nothing -> return (Nothing, go b)
{-# INLINE scan #-}

reserve :: (Monoid r, MonadSink (Source r s) m)
    => (s -> Producer r (Maybe t) m ()) -> Producer r (Maybe t) m ()
reserve k = Producer $ \cont -> Tap $ \r -> receiving (\t -> unTap t r) >>= \case
  Nothing -> return (Nothing, cont ())
  Just s -> unTap (unProducer (k s) cont) mempty

map :: (Functor t, Monad m) => (a -> b) -> Distiller (Tap r (t a)) r (t b) m
map = mapping . fmap
{-# INLINE map #-}

map' :: (Functor t, Monad m) => (a -> b) -> Distiller (Tap r (t a)) r (t b) m
map' f = traversing $ (pure$!) . fmap f
{-# INLINE map' #-}

concatMap :: (Foldable f, Monad m) => (a -> f b) -> Pipe a b m
concatMap f = go where
  go = reservingTap $ \case
    Just a -> unTap (foldr (consTap . Just) go (f a)) mempty
    Nothing -> return (Nothing, go)
{-# INLINE concatMap #-}

filter :: Monad m => (a -> Bool) -> Pipe a a m
filter p = filtering (maybe True p)
{-# INLINE filter #-}

mapAccum :: Monad m => (s -> a -> (s, b)) -> s -> Pipe a b m
mapAccum f x = go x where
  go s = reservingTap $ \case
    Just a -> let (s', b) = f s a in return (Just b, go s')
    Nothing -> return (Nothing, go s)
{-# INLINE mapAccum #-}

traverse :: (Monad m) => (a -> m b) -> Pipe a b m
traverse f = traversing (Prelude.traverse f)
{-# INLINE traverse #-}

take :: Monad m => Int -> Pipe a a m
take x = go x where
  go 0 = repeatTap Nothing
  go n = reservingTap $ \a -> return (a, go (n - 1))
{-# INLINE take #-}

drop :: Monad m => Int -> Pipe a a m
drop n = makeTap $ do
  replicateM_ n consume
  return echo
{-# INLINE drop #-}

takeWhile :: Monad m => (a -> Bool) -> Pipe a a m
takeWhile p = go where
  go = reservingTap $ \case
    Just s | p s -> return (Just s, go)
    _ -> return (Nothing, go)
{-# INLINE takeWhile #-}

dropWhile :: Monad m => (a -> Bool) -> Pipe a a m
dropWhile p = go where
  go = reservingTap $ \case
    Just s | p s -> unTap go mempty
    x -> return (x, go)
{-# INLINE dropWhile #-}

-- | Consume all the content of a 'Tap' and return the elements as a list.
drinkUp :: (Monoid r, Semigroup r, MonadSink (Tap r (Maybe s)) m) => m [s]
drinkUp = go id where
  go f = consume >>= maybe (pure $ f []) (\x -> go $ f . (x:))
{-# INLINE drinkUp #-}
