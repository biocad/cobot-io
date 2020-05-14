module Bio.ABI.Clean
  ( Cleanable (..)

  , Thresholds (..)
  , defaultThresholds
  ) where

import           Control.Monad (guard, join)
import qualified Data.Vector   as V

import           Bio.Sequence            (mean, meanInRange)
import qualified Bio.Sequence            as S (drop, length, reverse, tail, take)
import           Bio.Sequence.Basecalled (BasecalledSequence, BasecalledSequenceWithRawData (..))

-- | Ability to clean initial data.
-- Returns cleaned variant or 'Nothing' if it can not be cleaned.
--
class Cleanable a where
  clean :: a -> Maybe a
  clean = cleanWith defaultThresholds

  cleanWith :: Thresholds -> a -> Maybe a

-- | Thresholds to clean the data.
--
-- ABI file contains sequence with quality.
-- By design of sanger sequencing method start and end of the sequence have bad quality.
-- Moreover, internal part of the sequence can also has bad quality.
-- To clean the data we make 2 steps.
--
-- Step 1. Clean edges:
--
--   * take frame with @frameSize@ and go through the sequence;
--   * on each step evaluate mean value;
--   * if mean value less than 'edgeThreshold', go further;
--   * if mean value more than 'edgeThreshold' stop and cut the sequence from END of this frame;
--   * repeat this algorithm for the right edge.
--
-- Step 2. Evaluate quality:
--
--   * for cropped sequence evaluate mean value;
--   * if mean value less then 'innerThreshold', sequence is bad;
--   * if mean value more then 'innerThreshold', sequence is acceptable.
--
-- Logic of this algorithm and 'defaultThresholds' were obtained by taking experiments with read ABI files.
--
data Thresholds
  = Thresholds
      { frameSize      :: Int
      , edgeThreshold  :: Double
      , innerThreshold :: Double
      }
  deriving (Eq, Show)

-- | These thresholds were selected by many experiments on ab1-files.
--
defaultThresholds :: Thresholds
defaultThresholds = Thresholds 10 20 30

instance Cleanable BasecalledSequence where
  cleanWith thr input = do
      cut <- fromBoth
      guard $ checkInner thr cut
      return cut
    where
      fromLeft = doCutEdge thr input
      fromBoth =  fmap S.reverse
               .  join
               $  doCutEdge thr
               .  S.reverse
              <$> fromLeft

instance Cleanable BasecalledSequenceWithRawData where
  cleanWith thr input@BasecalledSequenceWithRawData{..} = do
    toDropLeft <- cutEdge thr bsSequence
    let leftDroppedSequ = S.drop toDropLeft bsSequence
    let leftDroppedPloc = V.drop toDropLeft bsPeakLocations

    toDropRight <- cutEdge thr $ S.reverse leftDroppedSequ
    let rightDroppedSequ = S.take (S.length leftDroppedSequ - toDropRight) leftDroppedSequ
    let rightDroppedPloc = V.take (V.length leftDroppedPloc - toDropRight) leftDroppedPloc

    return input { bsSequence = rightDroppedSequ, bsPeakLocations = rightDroppedPloc }

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

checkInner :: Thresholds -> BasecalledSequence -> Bool
checkInner Thresholds{..} = (> innerThreshold) . mean

doCutEdge :: Thresholds -> BasecalledSequence -> Maybe BasecalledSequence
doCutEdge t sequ = do
    toDrop <- cutEdge t sequ
    return $ S.drop toDrop sequ

cutEdge :: Thresholds -> BasecalledSequence -> Maybe Int
cutEdge t@Thresholds{..} sequ | S.length sequ < frameSize                    = Just 0
                              | meanInR < edgeThreshold && S.length sequ > 1 = (1+) <$> cutEdge t (S.tail sequ)
                              | S.length sequ > frameSize                    = Just frameSize
                              | otherwise                                    = Nothing
  where
    meanInR = meanInRange sequ (0, frameSize - 1)
