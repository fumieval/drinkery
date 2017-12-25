{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
import qualified Data.Drinkery as D
import qualified Data.Drinkery.Glass as D
import Data.Functor.Identity
import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.List
import Data.Void

drainD :: D.Still () (Maybe Int) IO () (Maybe a) -> IO ()
drainD h = sourceD D.+& h D.$& D.sinkNull

value :: Int
value = 10000

sourceD :: (Monoid r, Monad m) => D.Tap r (Maybe Int) m
sourceD = D.runSommelier $ D.taste [1..value]

main = defaultMain
  [ bgroup "scan"
      [ bench "drinkery" $ whnfIO $ drainD (D.scanningMaybe (+) 0)
      ]
  ]
