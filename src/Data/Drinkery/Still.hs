{-# LANGUAGE BangPatterns, Rank2Types, FlexibleContexts, LambdaCase #-}
module Data.Drinkery.Still where

import Control.Applicative
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
  go b = Tap $ \r -> Drinker $ \tap -> do
    (m, t') <- unTap tap r
    case m of
      Just a -> let !b' = f b a in return ((Just b', go b'), t')
      Nothing -> return ((Nothing, go b), t')
{-# INLINE scan #-}

reserve :: (Monoid r, MonadDrunk (Cask r s) m)
    => (s -> Barman r (Maybe t) m ()) -> Barman r (Maybe t) m ()
reserve k = Barman $ \cont -> Tap $ \r -> drinking (\t -> unTap t r) >>= \case
  Nothing -> return (Nothing, cont ())
  Just s -> unTap (unBarman (k s) cont) mempty

map :: (Functor t, Monad m) => (a -> b) -> Distiller (Tap r (t a)) r (t b) m
map = mapping . fmap
{-# INLINE map #-}

mapMaybe :: (Monad m) => (a -> Maybe b) -> Pipe a b m
mapMaybe f = inexhaustible $ reserve $ mapM_ yield . f
{-# INLINE mapMaybe #-}

filter :: Monad m => (a -> Bool) -> Pipe a a m
filter = filtering . maybe True
{-# INLINE filter #-}

mapAccum :: Monad m => (s -> a -> (s, b)) -> s -> Pipe a b m
mapAccum f = go where
  go s = Tap $ \r -> Drinker $ \tap -> do
    (m, t') <- unTap tap r
    case m of
      Just a -> let (s', b) = f s a in return ((Just b, go s'), t')
      Nothing -> return ((Nothing, go s), t')
{-# INLINE mapAccum #-}

-- | Consume all the content of a 'Tap' and return the elements as a list.
drinkUp :: (Monoid r, Semigroup r, MonadDrunk (Tap r (Maybe s)) m) => m [s]
drinkUp = drink >>= maybe (pure []) (\x -> (x:) <$> drinkUp)

sip :: (Monoid r, Alternative m, MonadDrunk (Tap r (Maybe s)) m) => m s
sip = drink >>= maybe empty pure
{-# INLINE sip #-}
