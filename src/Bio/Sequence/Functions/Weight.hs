{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Bio.Sequence.Functions.Weight
  ( mean, meanInRange
  , getWeight, unsafeGetWeight
  , getWeights
  , toWeighted, unsafeToWeighted
  , drop, take, tail
  ) where

import           Bio.Sequence.Class              (ContainsWeight,
                                                  IsBareSequence,
                                                  IsSequence (..),
                                                  IsWeight (..),
                                                  IsWeightedSequence, sequ,
                                                  unsafeWeightedSequence,
                                                  weightedSequence, weights,
                                                  _sequenceInner)
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
    indexError = "Bio.Seqence.Weight: index out of range."

unsafeGetWeight :: ContainsWeight s => s -> Int -> Weight s
unsafeGetWeight sequ' = unsafeEither . getWeight sequ'

-- | Get 'Weight's of all elements in @s@.
--
getWeights :: (ContainsWeight s) => s -> [Weight s]
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

-- | Unsafe drop:
--     * if n < 0, an error is thrown;
--     * if n >= length @s@, an error is thrown.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] mempty [0.1, 0.2, 0.3, 0.4]
-- > drop 2 sequWeighted == Sequence [b', 'a'] mempty [0.3, 0.4]
-- > drop -1 sequWeighted == error
-- > drop 4 sequWeighted == error
--
drop :: IsWeightedSequence s => Int -> s -> s
drop n (toSequence -> s) | n < 0         = error "Bio.Sequence.Weight: drop with negative value."
                         | n >= length s = error "Bio.Sequence.Weight: empty sequence as result of drop."
                         | otherwise     = res
  where
    droppedSequ = V.drop n $ s ^. sequ
    newWeights  = V.drop n $ s ^. weights

    res = fromSequence $ _sequenceInner droppedSequ mempty newWeights

-- | Unsafe take:
--     * if n < 0, an error is thrown;
--     * if n == 0, an error is thrown.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] mempty [0.1, 0.2, 0.3, 0.4]
-- > take 2 sequWeighted == Sequence ['a', 'a'] mempty [0.1, 0.2]
-- > take -1 sequWeighted == error
-- > take 0 sequWeighted == error
--
take :: IsWeightedSequence s => Int -> s -> s
take n (toSequence -> s) | n < 0     = error "Bio.Sequence.Weight: take with negative value."
                         | n == 0    = error "Bio.Sequence.Weight: empty sequence as result of take."
                         | otherwise = res
  where
    takenSequ  = V.take n $ s ^. sequ
    newWeights = V.take n $ s ^. weights

    res = fromSequence $ _sequenceInner takenSequ mempty newWeights

-- | Unsafe tail:
--     * length @s@ == 0, an error is thrown;
--     * length @s@ == 1, an error is thrown.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] mempty [0.1, 0.2, 0.3, 0.4]
-- > tail sequWeighted == Sequence [a', 'b', 'a'] mempty [0.2, 0.3, 0.4]
-- > tail (tail (tail (tail (tail sequWeighted)))) == error
--
tail :: IsWeightedSequence s => s -> s
tail s | length s == 0 = error "Bio.Sequence.Weight: tail from empty sequence."
       | otherwise     = drop 1 s

