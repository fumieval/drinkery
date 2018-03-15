{-# LANGUAGE BangPatterns, Rank2Types, FlexibleContexts #-}
module Data.Drinkery.Still where

import Control.Applicative
import Data.Drinkery.Class
import Data.Drinkery.Distiller
import Data.Drinkery.Tap
import Data.Semigroup

-- | Mono in/out
type Still p q r s m = Distiller (Tap p (Maybe q)) r (Maybe s) m

type SimpleStill a b m = forall r. (Monoid r, Semigroup r) => Still r a r b m

scan :: Monad m => (b -> a -> b) -> b -> SimpleStill a b m
scan f b0 = consTap (Just b0) $ go b0 where
  go b = Tap $ \r -> Drinker $ \tap -> do
    (m, t') <- unTap tap r
    case m of
      Just a -> let !b' = f b a in return ((Just b', go b'), t')
      Nothing -> return ((Nothing, go b), t')
{-# INLINE scan #-}

map :: (Functor t, Monad m) => (a -> b) -> Distiller (Tap r (t a)) r (t b) m
map = mapping . fmap
{-# INLINE map #-}

filter :: Monad m => (a -> Bool) -> SimpleStill a a m
filter = filtering . maybe True
{-# INLINE filter #-}

-- | Consume all the content of a 'Tap' and return the elements as a list.
drinkUp :: (Monoid r, Semigroup r, MonadDrunk (Tap r (Maybe s)) m) => m [s]
drinkUp = drink >>= maybe (pure []) (\x -> (x:) <$> drinkUp)

sip :: (Monoid r, Alternative m, MonadDrunk (Tap r (Maybe s)) m) => m s
sip = drink >>= maybe empty pure
{-# INLINE sip #-}