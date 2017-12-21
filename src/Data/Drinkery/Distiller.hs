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
  ( Spigot, Distiller
  -- * Special combinators
  , (+&)
  , ($&)
  -- * Basic combinators
  , (++$)
  , (++&)
  -- * Stock distillers
  , mapping
  , traversing
  , filtering
  , scanning
  , scanningMaybe
  , throughMaybe
  -- * Internal
  , boozeOn
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
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

-- | @Spigot m r s@ is a stream producer that produces @s@ receiving @r@.
type Spigot m = Tap (IdentityT m)

-- | @Distiller p q m r s@ is a stream transducer which has five parameters:
--
-- * @p@ request to the upstream
-- * @q@ input
-- * @m@ underlying monad
-- * @r@ request from the downstream
-- * @s@ output
--
-- This is also a 'Tap'.
--
type Distiller p q m = Tap (Patron p q m)

infix 6 +&
infixr 7 $&
infixr 7 ++&
infixl 8 ++$

-- | Connect a tap with a patron.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap (along with the result).
-- * @&@ Right operand is a patron.
(++&) :: (Monoid r, Monad m, Monad (t m), MonadTrans t) => Tap (t m) r s -> Patron r s m a -> t m (Tap (t m) r s, a)
d ++& b = boozeOn lift (\r s t a -> pure (orderTap r $ foldr consTap t s, a)) mempty [] d (runPatron b)
{-# INLINE (++&) #-}
{-# SPECIALISE INLINE (++&) :: (Monoid r, Monad m) => Spigot m r s -> Patron r s m a -> IdentityT m (Spigot m r s, a) #-}

-- | Attach a distiller to a tap.
--
-- Mnemonic:
--
-- * @+@ Left operand is a tap.
-- * @+@ Returns a tap.
-- * @$@ Right operand is a distiller.
--
-- @
-- (++$) :: Tap m p q -> Distiller p q m r s -> Tap m r s
-- (++$) :: Distiller p q m r s -> Distiller r s m t u -> Distiller p q m t u@
-- @
--
(++$) :: (Monoid p, Monad m, Monad (t m), MonadTrans t) => Tap (t m) p q -> Distiller p q m r s -> Tap (t m) r s
(++$) = go mempty [] where
  go r lo b (Tap m) = Tap $ \rs -> boozeOn lift
    (\r' lo' b' (s, cont) -> pure (s, go r' lo' b' cont))
    r lo b (runPatron (m rs))
{-# INLINE (++$) #-}

-- | Connect a spigot with a patron and close the used tap.
(+&) :: (Monoid r, CloseRequest r, Monad m) => Spigot m r s -> Patron r s m a -> m a
t +& b = runIdentityT $ do
  (t', a) <- t ++& b
  _ <- unTap t' closeRequest
  return a
{-# INLINE (+&) #-}

-- | Like '$&&' but discards the used distiller.
--
-- @($&) :: Distiller p q m r s -> Patron r s m a -> Patron p q m a@
--
($&) :: (Monoid r, Monad m, Monad (t m), MonadTrans t) => Tap (t m) r s -> Patron r s m a -> t m a
t $& b = fmap snd $ t ++& b
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
