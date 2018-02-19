{-# LANGUAGE FlexibleContexts #-}
module Data.Drinkery.Tie.Attoparsec where

import Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Data.Drinkery.Class
import Data.Drinkery.Tap

parseByteString :: (Monoid r, MonadDrunk (Tap r BS.ByteString) m) => Parser a -> m (Either ([String], String) a)
parseByteString m = drink >>= go . A.parse m
  where
    go (Done i r) = Right r <$ leftover i
    go (Partial f) = drink >>= go . f
    go (Fail bs contexts e) = Left (contexts, e) <$ leftover bs
