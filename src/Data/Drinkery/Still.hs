{-# LANGUAGE BangPatterns, Rank2Types, LambdaCase, FlexibleContexts, GeneralizedNewtypeDeriving, DeriveTraversable #-}
module Data.Drinkery.Still where

import Control.Applicative
import Data.Drinkery.Class
import Data.Drinkery.Distiller
import Data.Drinkery.Tap
import Data.Maybe
import Data.Semigroup
import Data.List

-- | Chunky stream representation; @Glass []@ indicates an EOF.
newtype Glass a = Glass { unGlass :: [a] } deriving (Functor, Applicative, Alternative, Foldable, Traversable)

type Cask r s = Tap r (Glass s)

-- | Mono in/out
type Still p q r s m = Distiller (Tap p (Glass q)) r (Glass s) m

type Pipe a b m = forall r. (Monoid r, Semigroup r) => Still r a r b m

reserve :: (Monoid r, MonadDrunk (Cask r s) m)
    => ([s] -> Barman r (Glass t) m ())
    -> Barman r (Glass t) m ()
reserve k = Barman $ \cont -> Tap $ \r -> drinking (\t -> unTap t r) >>= \case
  Glass [] -> return (Glass [], cont ())
  Glass xs -> unTap (unBarman (k xs) cont) mempty

yield :: (Semigroup r, Applicative f, Applicative m) => s -> Barman r (f s) m ()
yield = pour . pure
{-# INLINE yield #-}

yieldMany :: (Applicative m, Semigroup r) => [s] -> Barman r (Glass s) m ()
yieldMany [] = pure ()
yieldMany xs = pour (Glass xs)

leftovers :: (Semigroup r, Monoid r, MonadDrunk (Cask r s) m) => [s] -> m ()
leftovers [] = pure ()
leftovers xs = leftover (Glass xs)

scan :: Monad m => (b -> a -> b) -> b -> Pipe a b m
scan f b0 = consTap (pure b0) $ go b0 where
  go b = Tap $ \r -> Drinker $ \tap -> do
    (Glass as, t') <- unTap tap r
    let (!b', bs) = mapAccumL (\x y -> let !z = f x y in (z, z)) b as
    return ((Glass bs, go b'), t')
{-# INLINE scan #-}

-- | @map :: (a -> b) -> 'Pipe' a b m@
map :: (Functor t, Monad m) => (a -> b) -> Distiller (Tap r (t a)) r (t b) m
map = mapping . fmap
{-# INLINE map #-}

mapMaybe :: Monad m => (a -> Maybe b) -> Pipe a b m
mapMaybe f = inexhaustible $ reserve $ \xs -> yieldMany $ Data.Maybe.mapMaybe f xs
{-# INLINE mapMaybe #-}

filter :: Monad m => (a -> Bool) -> Pipe a a m
filter f = inexhaustible $ reserve $ \xs -> yieldMany $ Prelude.filter f xs
{-# INLINE filter #-}

mapAccum :: (Monad m) => (s -> a -> (s, b)) -> s -> Pipe a b m
mapAccum f = go where
  go s = Tap $ const $ do
    Glass xs <- drink
    let (!s', ys) = mapAccumL f s xs
    return (Glass ys, go s')

-- | Consume all the content of a 'Tap' and return the elements as a list.
drinkUp :: (Monoid r, Semigroup r, MonadDrunk (Cask r s) m) => m [s]
drinkUp = drink >>= \case
  Glass [] -> pure []
  Glass xs -> (xs++) <$> drinkUp

sip :: (Monoid r, Semigroup r, MonadDrunk (Cask r s) m) => m (Maybe s)
sip = drink >>= \case
  Glass [] -> pure Nothing
  Glass (x : xs) -> Just x <$ leftovers xs
{-# INLINE sip #-}
