{-# LANGUAGE FlexibleContexts, DeriveTraversable #-}
module Data.Drinkery.Tie.ByteString where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Drinkery.Class
import Data.Drinkery.Tap
import Data.Functor.Identity
import Data.Foldable
import System.IO

data Delimited a = Chunk a | Delimit deriving (Functor, Foldable, Traversable)

-- | Split a stream of 'ByteString's to smaller chunks by lines.
lineChunks :: (Monoid r, Foldable f, MonadDrunk (Tap r (f B.ByteString)) m) => Tap r (Delimited B.ByteString) m
lineChunks = inexhaustible $ do
  accept >>= request
  bs <- toList <$> drink
  if not (null bs) && all B.null bs
    then pour $ Chunk B.empty
    else step bs
  where
    step (bs : bss) = do
      let (l, r) = B.break (=='\n') bs
      pour $ Chunk l
      if B.null r then step bss else do
        if B.head r == '\n'
          then pour Delimit >> step (B.tail r : bss)
          else step (r : bss)
    step [] = return ()

combined :: (Monoid r, Foldable f, MonadDrunk (Tap r (f B.ByteString)) m) => Tap r (Maybe B.ByteString) m

data HandleRequest = HSeek !SeekMode !Integer | HClose deriving (Show, Eq, Ord)

instance CloseRequest HandleRequest where
  closeRequest = HClose

hTapN :: MonadIO m => Int -> Handle -> Tap [HandleRequest] (Identity B.ByteString) m
hTapN n h = Tap go where
  go (HClose : _) = liftIO (hClose h) >> return (Identity B.empty, error "tapHandle: closed")
  go (HSeek m i : rs) = liftIO (hSeek h m i) >> go rs
  go [] = do
    c <- liftIO $ B.hGet h n
    return (Identity c, Tap go)

hTap :: MonadIO m => Handle -> Tap [HandleRequest] (Identity B.ByteString) m
hTap = hTapN 4080

tapFile :: MonadIO m => FilePath -> Tap [HandleRequest] (Identity B.ByteString) m
tapFile path = makeTap $ hTap <$> liftIO (openFile path ReadMode)
