{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Bio.Sequence.Functions.Sequence
  ( drop
  , getRange, unsafeGetRange
  , length, null
  , reverse
  , tail
  , take
  , toList
  , (!), (!?)
  ) where

import           Control.Lens
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Foldable        as F (length, null, toList)
import qualified Data.List            as L (drop, take)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Vector          as V
import           Prelude              hiding (drop, length, null, reverse, tail, take)


import Bio.NucleicAcid.Nucleotide (Complementary (..))
import Bio.Sequence.Class         (ContainsNoMarking, IsSequence (..), _sequenceInner, markings,
                                   sequ, weights)
import Bio.Sequence.Range         (Range (..), RangeBorder (..), checkRange, mapRange, swapRange)
import Bio.Sequence.Utilities     (unsafeEither)

-- | Get elements from sequence that belong to given 'Range'. If the range is a Span, then both lower and upper bounds are included.
-- If given 'Range' is out of bounds, an error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > getRange sequ (0, 3) == Just ['a', 'a', 'b']
--
getRange :: (IsSequence s, MonadError Text m, Complementary (Element s)) => s -> Range -> m [Element s]
getRange s r | checkRange (length s) r = pure $ extractRange s r 
             | otherwise               = throwError "Bio.Sequence.Functions.Sequence: invalid range in getRange."

extractRange :: (IsSequence s, Complementary (Element s)) => s -> Range -> [Element s]
extractRange s (Point pos)                                  = [s ! pos]
extractRange s (Span (RangeBorder _ lo) (RangeBorder _ hi)) = L.drop lo . L.take (hi + 1) . toList $ s
extractRange _ (Between _ _)                                = []
extractRange s (Join ranges)                                = concatMap (extractRange s) ranges
extractRange s (Complement range)                           = rcNA $ extractRange s range

unsafeGetRange :: (IsSequence s, Complementary (Element s)) => s -> Range -> [Element s]
unsafeGetRange s = unsafeEither . getRange s

-- | Unsafe operator to get elemnt at given position in @s@.
--
infixl 9 !
(!) :: IsSequence s => s -> Int -> Element s
(!) s = fromMaybe (error "Bio.Sequence.Functions.Sequence: index out of Sequence's length.") . (s !?)

-- | Safe operator to get element at given position in @s@.
--
infixl 9 !?
(!?) :: IsSequence s => s -> Int -> Maybe (Element s)
(!?) (toSequence -> s) = ((s ^. sequ) V.!?)

-- | List all elemnts of @s@.
--
toList :: IsSequence s => s -> [Element s]
toList = F.toList . toSequence

-- | Calculates length of @s@.
--
length :: IsSequence s => s -> Int
length = F.length . toSequence

-- | Returns 'True' if @s@ is empty. Returns 'False' otherwise.
--
null :: IsSequence s => s -> Bool
null = F.null . toSequence

-- | Reverses given 'IsSequence' @s@. 'Marking's and 'Weight's are reversed, too.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] [1, 2, 3, 4]
-- > reverse sequ == Sequence ['a', 'b', 'a', 'a'] [("Letter A", (2, 4)), ("Letter A", (0, 1)), ("Letter B", (1, 2))] [4, 3, 2, 1]
--
reverse :: IsSequence s => s -> s
reverse (toSequence -> s) = res
  where
    newMaxInd = length s - 1

    newSequ     = V.reverse $ s ^. sequ
    newMarkings = fmap (fmap $ swapRange . mapRange ((-) newMaxInd)) $ s ^. markings
    newWeights  = V.reverse $ s ^. weights

    res = fromSequence $ _sequenceInner newSequ newMarkings newWeights

-- | Unsafe drop:
--     * if n < 0, an error is thrown;
--     * if n >= length @s@, an error is thrown.
--
-- > sequWeighted = Sequence ['a', 'a', 'b', 'a'] mempty [0.1, 0.2, 0.3, 0.4]
-- > drop 2 sequWeighted == Sequence [b', 'a'] mempty [0.3, 0.4]
-- > drop (-1) sequWeighted == error
-- > drop 4 sequWeighted == error
--
drop :: ContainsNoMarking s => Int -> s -> s
drop n (toSequence -> s) | n < 0         = error "Bio.Sequence.Functions.Sequence: drop with negative value."
                         | n >= length s = error "Bio.Sequence.Functions.Sequence: empty sequence as result of drop."
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
take :: ContainsNoMarking s => Int -> s -> s
take n (toSequence -> s) | n < 0     = error "Bio.Sequence.Functions.Sequence: take with negative value."
                         | n == 0    = error "Bio.Sequence.Functions.Sequence: empty sequence as result of take."
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
tail :: ContainsNoMarking s => s -> s
tail s | length s == 0 = error "Bio.Sequence.Functions.Sequence: tail from empty sequence."
       | otherwise     = drop 1 s
