module Bio.ABI.Clean
  ( Cleanable (..)
  , Thresholds (..)
  ) where

import           Bio.Sequence  (Weighted (..))
import           Data.Foldable (Foldable (..))
import           Data.Array                 (Array, listArray)

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

instance Cleanable [Weighted Char] where
  cleanWith thr input = if checkInner thr fromBoth
                          then Just fromBoth
                          else Nothing
    where
      fromLeft = cutEdge defaultThresholds input
      fromBoth = reverse
               . cutEdge defaultThresholds
               . reverse
               $ fromLeft

instance Cleanable (Array Int (Weighted Char)) where
  cleanWith thr input = 
      case cleanWith thr . toList $ input of
          Just x  -> Just $ listArray (0, length x - 1) x
          Nothing -> Nothing

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

checkInner :: Thresholds -> [Weighted Char] -> Bool
checkInner Thresholds{..} = (> innerThreshold) . meanWeight

cutEdge :: Thresholds -> [Weighted a] -> [Weighted a]
cutEdge t@Thresholds{..} list | length list < frameSize = list
                              | meanWeight frameObjects < edgeThreshold = cutEdge t (tail list)
                              | otherwise = drop frameSize list
  where
    frameObjects = take frameSize list

meanWeight :: [Weighted a] -> Double
meanWeight = mean . fmap _weight

mean :: [Double] -> Double
mean l = sum l / fromIntegral (length l)
