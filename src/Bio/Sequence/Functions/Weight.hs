{-# LANGUAGE ViewPatterns #-}

module Bio.Sequence.Functions.Weight
  ( mean
  , meanInRange
  , getWeight
  , unsafeGetWeight
  , getWeights
  , toWeighted
  , unsafeToWeighted
  ) where

import           Bio.Sequence.Class              (ContainsWeight,
                                                  IsBareSequence,
                                                  IsSequence (..),
                                                  IsWeight (..),
                                                  IsWeightedSequence, sequ,
                                                  unsafeWeightedSequence,
                                                  weightedSequence, weights)
import           Bio.Sequence.Functions.Sequence (length)
import           Bio.Sequence.Utilities          (unsafeEither)
import           Control.Lens
import           Control.Monad.Except            (MonadError, throwError)
import           Data.Text                       (Text)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V (drop, length, take,
                                                       toList, (!))
import           Prelude                         hiding (drop, head, length,
                                                  null, reverse, tail, take,
                                                  (!!))

-- | Range of form [a, b].
--
type RangeInclusive = (Int, Int)

-- | Calculate mean weight for given @s@.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] someMarking [0.1, 0.2, 0.3, 0.4]
-- mean sequWeighted == 0.45
--
mean :: ContainsWeight s => s -> Double
mean s = meanInRange s (0, length s - 1)

-- | Calculate mean weight for given @s@ in range 'RangeInclusive'.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] someMarkings [0.1, 0.2, 0.3, 0.4]
-- > meanInRange sequWeighted (1, 2) == 0.25
--
meanInRange :: ContainsWeight s => s -> RangeInclusive -> Double
meanInRange (toSequence -> s) (lInd, rInd) = res
  where
    neededWeights = V.take (rInd + 1) $ V.drop lInd $ s ^. weights

    res = mean' $ fmap toDouble neededWeights

    mean' :: Vector Double -> Double
    mean' l = sum l / fromIntegral (V.length l)

-- | Get weight of element of @s@ at given position.
--
getWeight :: (ContainsWeight s, MonadError Text m) => s -> Int -> m (Weight s)
getWeight (toSequence -> s) i | i >= V.length ws = throwError indexError
                              | otherwise        = pure $ ws V.! i
  where
    ws = s ^. weights

    indexError :: Text
    indexError = "Bio.Sequence.Functions.Weight: index out of range."

unsafeGetWeight :: ContainsWeight s => s -> Int -> Weight s
unsafeGetWeight sequ' = unsafeEither . getWeight sequ'

-- | Get 'Weight's of all elements in @s@.
--
getWeights :: ContainsWeight s => s -> [Weight s]
getWeights = V.toList . (^. weights) . toSequence

-- | Converts 'IsBareSequence' @s@ to 'IsWeightedSequence' @s'@ that is weighted using provided list
-- of 'Weight's. If length of 'Weight's list is different from length of @s@, an
-- error is thrown.
--
-- > sequBare = Sequence ['a', 'a', 'b', 'a'] mempty mempty :: BareSequence Char
-- > toMarked sequ [0.1, 0.2, 0.3, 0.4] :: WeightedSequence Double Char
--
toWeighted :: (IsBareSequence s, IsWeightedSequence s', Weight s' ~ w, Element s ~ Element s', MonadError Text m) => s -> [w] -> m s'
toWeighted (toSequence -> s) = weightedSequence (V.toList $ s ^. sequ)

unsafeToWeighted :: (IsBareSequence s, IsWeightedSequence s', Weight s' ~ w, Element s ~ Element s') => s -> [w] -> s'
unsafeToWeighted (toSequence -> s) = unsafeWeightedSequence (V.toList $ s ^. sequ)

