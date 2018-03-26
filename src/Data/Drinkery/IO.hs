module Data.Drinkery.IO where

import Control.Exception
import Data.Drinkery.Class
import Data.Drinkery.Tap
import Data.IORef

-- | Create a popper from a 'Tap'.
--
-- @popperTap :: CloseRequest r => Tap r s IO -> GivesPopper@
--
popperTap :: (Monoid r, CloseRequest r) => Tap r s IO -> (IO s -> IO ()) -> IO ()
popperTap tap0 needsPopper = do
  vTap <- newIORef tap0
  let popper = do
        t <- readIORef vTap
        (s, t') <- unTap t mempty
        writeIORef vTap t'
        return s
  needsPopper popper `finally` do readIORef vTap >>= close
