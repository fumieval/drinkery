{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
import qualified Data.Drinkery as D
import Data.Functor.Identity
import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.List
import Data.Void
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified ListT as L
import qualified Control.Monad.Trans.List as L

drainD :: Monad m => D.Drinker (D.Tap () (Maybe a)) m ()
drainD = D.drainFrom D.drink

sourceAlt :: Monad m => ([Int] -> m Int) -> m Int
sourceAlt k = do
  a <- k [1..20]
  b <- k [1..a]
  c <- k [1..b]
  return $! a + b + c
{-# INLINE sourceAlt #-}

sourceSeq :: Monad m => (Int -> m ()) -> m ()
sourceSeq k = forM_ [1..20] $ \a -> forM_ [1..a] $ \b -> forM_ [1..b] $ \c -> k $! a + b + c
{-# INLINE sourceSeq #-}

sourceD :: Monad m => D.Tap () (Maybe Int) m
sourceD = D.runSommelier (sourceAlt D.taste)
{-# INLINE sourceD #-}

sourceP :: Monad m => P.Producer Int m ()
sourceP = sourceSeq P.yield
{-# INLINE sourceP #-}

main = defaultMain
  [ bgroup "drain"
      [ bench "drinkery/Barman" $ whnfIO
          $ apply (D.+& drainD)
          $ D.runBarman (sourceSeq (D.yield . pure))
      , bench "drinkery/Sommelier" $ whnfIO
          $ apply (D.+& drainD) sourceD
      , bench "pipes/Producer" $ whnfIO
          $ apply (\s -> P.runEffect $ s P.>-> P.drain) sourceP
      , bench "pipes/ListT" $ whnfIO
          $ apply (\s -> P.runEffect $ s P.>-> P.drain)
          $ P.enumerate (sourceAlt (P.Select . P.each))
      , bench "list-t" $ whnfIO
          $ apply (L.traverse_ $ const $ pure ())
          $ sourceAlt L.fromFoldable
      , bench "ListT" $ whnfIO
          $ apply (L.foldlM (\_ _ -> pure ()) ())
          $ sourceAlt L.fromList
      ]
  , bgroup "scan"
      [ bench "drinkery" $ whnfIO $ apply (\h -> sourceD D.+& h D.$& drainD)
          $ D.scanningMaybe (+) 0
      , bench "pipes" $ whnfIO $ apply (\p -> P.runEffect $ P.for (sourceP P.>-> p) P.discard)
          $ P.scan (+) 0 id
      ]
  ]

apply :: (a -> b) -> a -> b
apply f x = f x
{-# NOINLINE apply #-}
