{-# LANGUAGE Rank2Types, BangPatterns, LambdaCase, FlexibleContexts #-}
module Data.Drinkery.Distiller where

import Control.Monad.Trans.Class
import Data.Drinkery.Tap
import Data.Drinkery.Class
import Data.Drinkery.Boozer

boozeOn :: (Monoid r, Monad m) => (forall x. n x -> m x)
  -> (Tap m r s -> r -> [s] -> a -> m z)
  -> r -> [s] -> Tap m r s
  -> Boozer r s n a
  -> m z
boozeOn t cont = go where
  go !r [] (Tap f) (Drink k) = f r >>= \(s, b) -> go mempty [] b (k s)
  go !r (s : ss) b (Drink f) = go r ss b (f s)
  go !r ss b (Spit s k) = go r (s : ss) b k
  go !r ss b (Call r' k) = go (mappend r r') ss b k
  go !r ss b (Lift m) = t m >>= go r ss b
  go !r ss b (Pure a) = cont b r ss a
{-# INLINE boozeOn #-}

-- | Stream transducer
type Distiller p q m = Tap (Patron p q m)

distillWith :: (Monoid p, Monad m) => (forall x. n x -> m x) -> Tap m p q -> Distiller p q n r s -> Tap m r s
distillWith trans = go mempty [] where
  go r lo b (Tap m) = Tap $ \rs -> boozeOn trans
    (\b' r' lo' (s, cont) -> pure (s, go r' lo' b' cont))
    r lo b (runPatron (m rs))
{-# INLINE distillWith #-}

infix 6 +&
infixr 7 $&
infix 6 +-&
infixl 7 ++$
infixr 7 $-&
infixl 8 $$$

-- | Connect a tap with a boozer.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @-@ Returns a triple of the used distiller, leftovers, and the result.
-- * @&@ Right operand is a boozer.
(+-&) :: (Monoid r, Monad m) => Tap m r s -> Patron r s m a -> m (Tap m r s, a)
t +-& p = boozeOn id (\t r s a -> pure (orderTap r $ foldr consTap t s, a)) mempty [] t (runPatron p)
{-# INLINE (+-&) #-}

-- | Attach a distiller to a tap.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap.
-- * @$@ Right operand is a distiller.
(++$) :: (Monoid p, Monad m) => Tap m p q -> Distiller p q m r s -> Tap m r s
(++$) = distillWith id
{-# INLINE (++$) #-}

-- | Connect two distillers.
--
-- Mnemonic:
--
-- * @$@ Left operand is a distiller.
-- * @$@ Returns a distiller.
-- * @$@ Right operand is a distiller.
($$$) :: (Monoid r, Monad m) => Distiller p q m r s -> Distiller r s m t u -> Distiller p q m t u
($$$) = distillWith lift
{-# INLINE ($$$) #-}

-- | Attach a distiller to a boozer.
--
-- Mnemonic:
--
-- * @$@ Left operand is a distiller.
-- * @-@ Returns a triple of the used distiller, leftovers, and the result.
-- * @&@ Right operand is a boozer.
($-&) :: (Monoid r, Monad m) => Distiller p q m r s -> Patron r s m a
  -> Patron p q m (Distiller p q m r s, a)
d $-& b = boozeOn lift (\t r s a -> pure (orderTap r $ foldr consTap t s, a)) mempty [] d (runPatron b)
{-# INLINE ($-&) #-}

-- | Connect a tap with a boozer and discard the leftovers.
(+&) :: (Monoid r, Monad m) => Tap m r s -> Patron r s m a -> m a
t +& b = fmap snd $ t +-& b
{-# INLINE (+&) #-}

-- | Attach nd discard the leftovers.
($&) :: (Monoid r, Monad m) => Distiller p q m r s -> Patron r s m a -> Patron p q m a
t $& b = fmap snd $ t $-& b
{-# INLINE ($&) #-}

-- | Create a request-preserving distiller.
propagating :: Functor m => Patron r a m (b, Distiller r a m r b) -> Distiller r a m r b
propagating m = Tap $ \r -> call r >> m
{-# INLINE propagating #-}

mapping :: Functor m => (a -> b) -> Distiller r a m r b
mapping f = propagating $ drink >>= \a -> return (f a, mapping f)

traversing :: Monad m => (a -> m b) -> Distiller r a m r b
traversing f = propagating $ drink >>= \a -> lift (f a) >>= \b -> return (b, traversing f)

filtering :: (Monoid r, Functor m) => (a -> Bool) -> Distiller r a m r a
filtering f = propagating $ drink >>= \a -> if f a
  then return (a, filtering f)
  else unTap (filtering f) mempty

scanning :: (Monoid r, Functor m) => (b -> a -> b) -> b -> Distiller r a m r b
scanning f b0 = consTap b0 $ go b0 where
  go b = propagating $ fmap (\a -> let !b' = f b a in (b', go $ b')) drink
{-# INLINE scanning #-}

scanningMaybe :: (Monoid r, Functor m) => (b -> a -> b) -> b -> Distiller r (Maybe a) m r (Maybe b)
scanningMaybe f b0 = consTap (Just b0) $ go b0 where
  go b = Tap $ \r -> Patron $ \cont -> Call r $ Drink $ \case
    Just a -> let !b' = f b a in cont (Just b', go b')
    Nothing -> cont (Nothing, go b)
{-# INLINE scanningMaybe #-}
{-
-- | Transform a 'Distiller' to operate on a stream of 'Maybe's.
throughMaybe :: (Monoid r, Monad m) => Distiller p q m r s -> Distiller p (Maybe q) m r (Maybe s)
throughMaybe d = Tap $ \rs -> unPatron (iterBoozer
  (\qs (s, d') -> (Just s, throughMaybe d') <$ mapM_ (spit . Just) qs)
  (\cont -> drink >>= maybe (pure end) cont)
  (\p cont -> call p >> cont)
  ((>>=) . lift)
  (unTap d rs)) (Pure end) Pure
  where
    end = (Nothing, throughMaybe d)
-}
