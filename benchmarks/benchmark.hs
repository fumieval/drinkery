import Control.Arrow
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Void
import Gauge.Main
import qualified Data.Drinkery as D
import qualified Data.Drinkery.Glass as D
import qualified ListT as L
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Machine as M

drainD :: Monad m => D.Drinker (D.Cask () a) m ()
drainD = D.drainFrom D.drink

sourceAlt :: Monad m => ([Int] -> m Int) -> m Int
sourceAlt k = do
  a <- k [1..50]
  b <- k [1..a]
  c <- k [1..b]
  return $! a + b + c
{-# INLINE sourceAlt #-}

sourceSeq :: Monad m => (Int -> m ()) -> m ()
sourceSeq k = forM_ [1..50] $ \a -> forM_ [1..a] $ \b -> forM_ [1..b] $ \c -> k $! a + b + c
{-# INLINE sourceSeq #-}

sourceD :: Monad m => D.Cask () Int m
sourceD = D.runSommelier (sourceAlt D.taste)
{-# INLINE sourceD #-}

sourceP :: Monad m => P.Producer Int m ()
sourceP = sourceSeq P.yield
{-# INLINE sourceP #-}

sourceC :: Monad m => C.ConduitT i Int m ()
sourceC = sourceSeq C.yield
{-# INLINE sourceC #-}

sourceM :: M.Source Int
sourceM = M.construct $ sourceSeq M.yield
{-# INLINE sourceM #-}

main = defaultMain
  [ bgroup "drain"
      [ bench "drinkery/Barman" $ whnfIO
          $ apply (D.+& drainD)
          $ D.runBarman (sourceSeq D.yield)
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
      ]
  , bgroup "scan"
      [ bench "drinkery" $ whnfIO $ apply (\h -> sourceD D.+& h D.$& drainD)
          $ D.scan (+) 0
      , bench "pipes" $ whnfIO $ apply (\p -> P.runEffect $ P.for (sourceP P.>-> p) P.discard)
          $ P.scan (+) 0 id
      , bench "conduit" $ whnfIO $ C.runConduit $ apply (\(h, s) -> sourceC C..| h C..| s)
          (CC.scanl (+) 0, CC.sinkNull)
      , bench "machines" $ whnfIO $ apply (M.runT_ . (sourceM M.~>))
          $ M.scan (+) 0
      ]
   , bgroup "scan-chain"
      [ bench "drinkery/++$" $ whnfIO $ apply (\h -> D.runBarman (sourceSeq D.yield) D.+& h D.$& drainD)
          $ D.scan (+) 0 D.++$ D.scan (+) 0
      , bench "drinkery/$&" $ whnfIO $ apply (\(h, h') -> D.runBarman (sourceSeq D.yield) D.+& h D.$& h' D.$& drainD)
          (D.scan (+) 0, D.scan (+) 0)
      , bench "pipes" $ whnfIO $ apply (\p -> P.runEffect $ P.for (sourceP P.>-> p) P.discard)
          $ P.scan (+) 0 id P.>-> P.scan (+) 0 id
      , bench "conduit" $ whnfIO $ apply (\(h, s) -> C.runConduit $ sourceC C..| h C..| s)
          (CC.scanl (+) 0 C..| CC.scanl (+) 0, CC.sinkNull)
      , bench "machines" $ whnfIO $ apply (M.runT_ . (sourceM M.~>))
          $ M.scan (+) 0 M.~> M.scan (+) 0
      ]
  ]

apply :: (a -> b) -> a -> b
apply f x = f x
{-# NOINLINE apply #-}
