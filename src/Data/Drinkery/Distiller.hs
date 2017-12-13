{-# LANGUAGE Rank2Types, BangPatterns, LambdaCase, FlexibleContexts #-}
-----------------------------------------------------------------------
--
-- Module      :  Data.Drinkery.Distiller
-- Copyright   :  (c) Fumiaki Kinoshita 2017
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Stream transducers
-----------------------------------------------------------------------
module Data.Drinkery.Distiller
  ( Distiller
  -- * Special combinators
  , (+&)
  , ($&)
  -- * Attaching a distiller
  , (++$)
  , ($$$)
  -- * Patron
  , (++&)
  , ($$&)
  -- * Stock distillers
  , mapping
  , traversing
  , filtering
  , scanning
  , scanningMaybe
  , throughMaybe
  -- * Internal
  , boozeOn
  , distillWith
  ) where

import Control.Monad.Trans.Class
import Data.Drinkery.Tap
import Data.Drinkery.Class
import Data.Drinkery.Boozer

boozeOn :: (Monoid r, Monad m) => (forall x. n x -> m x)
  -> (r -> [s] -> Tap m r s -> a -> m z)
  -> r -> [s] -> Tap m r s
  -> Boozer r s n a
  -> m z
boozeOn t cont = go where
  go !r [] (Tap f) (Drink k) = f r >>= \(s, b) -> go mempty [] b (k s)
  go !r (s : ss) b (Drink f) = go r ss b (f s)
  go !r ss b (Spit s k) = go r (s : ss) b k
  go !r ss b (Call r' k) = go (mappend r r') ss b k
  go !r ss b (Lift m) = t m >>= go r ss b
  go !r ss b (Pure a) = cont r ss b a
{-# INLINE boozeOn #-}

-- | 'Distiller p q m r s' is a stream transducer which has five parameters:
--
-- * @p@ request to the upstream
-- * @q@ input
-- * @m@ underlying monad
-- * @r@ request from the downstream
-- * @s@ output
type Distiller p q m = Tap (Patron p q m)

distillWith :: (Monoid p, Monad m) => (forall x. n x -> m x) -> Tap m p q -> Distiller p q n r s -> Tap m r s
distillWith trans = go mempty [] where
  go r lo b (Tap m) = Tap $ \rs -> boozeOn trans
    (\r' lo' b' (s, cont) -> pure (s, go r' lo' b' cont))
    r lo b (runPatron (m rs))
{-# INLINE distillWith #-}

infix 6 +&
infixr 7 $&
infix 6 ++&
infixl 7 ++$
infixr 7 $$&
infixl 8 $$$

-- | Connect a tap with a patron.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap (along with the result).
-- * @&@ Right operand is a patron.
(++&) :: (Monoid r, Monad m) => Tap m r s -> Patron r s m a -> m (Tap m r s, a)
t ++& p = boozeOn id (\r s t' a -> pure (orderTap r $ foldr consTap t' s, a)) mempty [] t (runPatron p)
{-# INLINE (++&) #-}

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

-- | Full duplex composition of distillers.
--
-- Mnemonic:
--
-- * @$@ Left operand is a distiller.
-- * @$@ Returns a distiller.
-- * @$@ Right operand is a distiller.
($$$) :: (Monoid r, Monad m) => Distiller p q m r s -> Distiller r s m t u -> Distiller p q m t u
($$$) = distillWith lift
{-# INLINE ($$$) #-}

-- | Attach a distiller to a patron.
--
-- Mnemonic:
--
-- * @$@ Left operand is a distiller.
-- * @$@ Returns the used distiller.
-- * @&@ Right operand is a patron.
($$&) :: (Monoid r, Monad m) => Distiller p q m r s -> Patron r s m a
  -> Patron p q m (Distiller p q m r s, a)
d $$& b = boozeOn lift (\r s t a -> pure (orderTap r $ foldr consTap t s, a)) mempty [] d (runPatron b)
{-# INLINE ($$&) #-}

-- | Connect a tap with a patron and close the used tap.
(+&) :: (Monoid r, CloseRequest r, Monad m) => Tap m r s -> Patron r s m a -> m a
t +& b = do
  (t', a) <- t ++& b
  _ <- unTap t' closeRequest
  return a
{-# INLINE (+&) #-}

-- | Like '$&&' but discards the used distiller.
($&) :: (Monoid r, Monad m) => Distiller p q m r s -> Patron r s m a -> Patron p q m a
t $& b = fmap snd $ t $$& b
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

-- | Transform a 'Distiller' to operate on a stream of 'Maybe's.
throughMaybe :: (Monoid r, Monad m) => Distiller p q m r s -> Distiller p (Maybe q) m r (Maybe s)
throughMaybe d = Tap $ \rs -> iterBoozer
  (\qs (s, d') -> (Just s, throughMaybe d') <$ mapM_ (spit . Just) qs)
  (\cont -> drink >>= maybe (pure end) cont)
  (\p cont -> call p >> cont)
  ((>>=) . lift)
  $ runPatron $ unTap d rs
  where
    end = (Nothing, throughMaybe d)
