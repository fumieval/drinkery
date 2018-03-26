{-# LANGUAGE BangPatterns, Rank2Types, FlexibleContexts, LambdaCase #-}
module Data.Drinkery.Still where

import Control.Applicative
import Control.Monad (replicateM_)
import Data.Drinkery.Class
import Data.Drinkery.Distiller
import Data.Drinkery.Tap
import Data.Semigroup

type Cask r s = Tap r (Maybe s)

-- | Mono in/out
type Still p q r s m = Cask r s (Drinker (Cask p q) m)

type Pipe a b m = forall r. (Monoid r, Semigroup r) => Still r a r b m

scan :: Monad m => (b -> a -> b) -> b -> Pipe a b m
scan f b0 = consTap (Just b0) $ go b0 where
  go b = reservingTap $ \case
    Just a -> let !b' = f b a in return (Just b', go b')
    Nothing -> return (Nothing, go b)
{-# INLINE scan #-}

reserve :: (Monoid r, MonadDrunk (Cask r s) m)
    => (s -> Producer r (Maybe t) m ()) -> Producer r (Maybe t) m ()
reserve k = Producer $ \cont -> Tap $ \r -> drinking (\t -> unTap t r) >>= \case
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
filter = filtering . maybe True
{-# INLINE filter #-}

mapAccum :: Monad m => (s -> a -> (s, b)) -> s -> Pipe a b m
mapAccum f = go where
  go s = reservingTap $ \case
    Just a -> let (s', b) = f s a in return (Just b, go s')
    Nothing -> return (Nothing, go s)
{-# INLINE mapAccum #-}

traverse :: (Monad m) => (a -> m b) -> Pipe a b m
traverse = traversing . Prelude.traverse
{-# INLINE traverse #-}

take :: Monad m => Int -> Pipe a a m
take = go where
  go 0 = repeatTap Nothing
  go n = reservingTap $ \a -> return (a, go (n - 1))
{-# INLINE take #-}

drop :: Monad m => Int -> Pipe a a m
drop n = makeTap $ do
  replicateM_ n drink
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
drinkUp :: (Monoid r, Semigroup r, MonadDrunk (Tap r (Maybe s)) m) => m [s]
drinkUp = go id where
  go f = drink >>= maybe (pure $ f []) (\x -> go $ f . (x:))
{-# INLINE drinkUp #-}

sip :: (Monoid r, Alternative m, MonadDrunk (Tap r (Maybe s)) m) => m s
sip = drink >>= maybe empty pure
{-# INLINE sip #-}
