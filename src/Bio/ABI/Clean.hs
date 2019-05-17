module Bio.ABI.Clean
  ( Cleanable (..)
  , Thresholds (..)
  ) where

import           Bio.ABI.Type  (ABIProcessed)
import           Bio.Sequence  (mean, meanInRange)
import qualified Bio.Sequence  as S (drop, length, reverse, tail)
import           Control.Monad (join)

-- | Ability to clean initial data.
-- Returns cleaned variant or 'Nothing' if it can not be cleaned.
--
class Cleanable a where
  clean :: a -> Maybe a
  clean = cleanWith defaultThresholds

  cleanWith :: Thresholds -> a -> Maybe a

-- | Thresholds to clean the data.
-- ABI file contains sequence with quality.
-- By design of sanger sequencing method start and end of the sequence have bad quality.
-- Moreover, internal part of the sequence can also has bad quality.
-- To clean the data we make 2 steps.
--
-- Step 1. Clean edges:
--   * take frame with @frameSize@ and go through the sequence;
--   * on each step evaluate mean value;
--   * if mean value less than @edgeThreshold@, go further;
--   * if mean value more than @edgeThreshold@, stop and cut the sequence from END of this frame;
--   * repeat this algorithm for the right edge.
--
-- Step 2. Evaluate quality:
--   * for cropped sequence evaluate mean value;
--   * if mean value less then @innerThreshold@, sequence is bad;
--   * if mean value more then @innerThreshold@, sequence is acceptable.
--
-- Logic of this algorithm and 'defaultThresholds' were obtained by taking experiments with read ABI files.
--
data Thresholds
   = Thresholds { frameSize      :: Int
                , edgeThreshold  :: Double
                , innerThreshold :: Double
                }
  deriving (Eq, Show)

-- | This thresholds were selected by many experiments on ab1-files.
--
defaultThresholds :: Thresholds
defaultThresholds = Thresholds 10 20 30

instance Cleanable ABIProcessed where
  cleanWith thr input = if fmap (checkInner thr) fromBoth == Just True
                          then fromBoth
                          else Nothing
    where
      fromLeft = cutEdge defaultThresholds input
      fromBoth =  fmap S.reverse
               .  join
               $  cutEdge defaultThresholds
               .  S.reverse
              <$> fromLeft

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

checkInner :: Thresholds -> ABIProcessed -> Bool
checkInner Thresholds{..} = (> innerThreshold) . mean

cutEdge :: Thresholds -> ABIProcessed -> Maybe ABIProcessed
cutEdge t@Thresholds{..} sequ | S.length sequ < frameSize                    = Just sequ
                              | meanInR < edgeThreshold && S.length sequ > 1 = cutEdge t $ S.tail sequ
                              | S.length sequ > frameSize                    = Just $ S.drop frameSize sequ
                              | otherwise                                    = Nothing
  where
    meanInR = meanInRange sequ (0, frameSize - 1)
