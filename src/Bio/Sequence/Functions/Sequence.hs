{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Bio.Sequence.Functions.Sequence
  ( getRange, unsafeGetRange
  , length, null
  , reverse
  , toList
  , (!), (!?)
  ) where

import           Bio.Sequence.Class     (IsSequence (..), markings, sequ,
                                         weights, _sequenceInner)
import           Bio.Sequence.Utilities (Range, checkRange, unsafeEither)
import           Control.Lens
import           Control.Monad.Except   (MonadError, throwError)
import           Data.Bifunctor         (bimap)
import qualified Data.List              as L (drop, length, take)
import           Data.Text              (Text)
import           Data.Tuple             (swap)
import qualified Data.Vector            as V (reverse, toList, (!))
import           Prelude                hiding (length, null, reverse)

-- | Get elements from sequence that belong to given 'Range' (format of range is [a; b)).
-- If given 'Range' is out of bounds, an error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > getRange sequ (0, 3) == Just ['a', 'a', 'b']
--
getRange :: (IsSequence s, MonadError Text m) => s -> Range -> m [Element s]
getRange s r@(lInd, rInd) | checkRange (length s) r = pure $ L.take (rInd - lInd) $ L.drop lInd $ toList s
                          | otherwise               = throwError "Bio.Sequence: invalid range in getRange."

unsafeGetRange :: IsSequence s => s -> Range -> [Element s]
unsafeGetRange s = unsafeEither . getRange s

-- | Unsafe operator to get elemnt at given position in @s@.
--
infixl 9 !
(!) :: IsSequence s => s -> Int -> Element s
(!) (toSequence -> s) = ((s ^. sequ) V.!)

-- | Safe operator to get element at given position in @s@.
--
infixl 9 !?
(!?) :: (IsSequence s, MonadError Text m) => s -> Int -> m (Element s)
(!?) s i | i >= length s || i < 0 = throwError "Bio.Sequence: index out of Sequence's length."
         | otherwise              = pure $ s ! i

-- | List all elemnts of @s@.
--
toList :: IsSequence s => s -> [Element s]
toList = V.toList . (^. sequ) . toSequence

-- | Calculates length of @s@.
--
length :: IsSequence s => s -> Int
length = L.length . (^. sequ) . toSequence

-- | Returns 'True' if @s@ is empty. Returns 'False' otherwise.
--
null :: IsSequence s => s -> Bool
null = (== 0) . length

-- | Reverses given 'IsSequence' @s@. 'Marking's and 'Weight's are reversed, too.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] [1, 2, 3, 4]
-- > reverse sequ == Sequence ['a', 'b', 'a', 'a'] [("Letter A", (2, 4)), ("Letter A", (0, 1)), ("Letter B", (1, 2))] [4, 3, 2, 1]
--
reverse :: IsSequence s => s -> s
reverse (toSequence -> s) = res
  where
    newMaxInd = length s

    newSequ     = V.reverse $ s ^. sequ
    newMarkings = fmap (fmap $ swap . bimap ((-) newMaxInd) ((-) newMaxInd)) $ s ^. markings
    newWeights  = V.reverse $ s ^. weights

    res = fromSequence $ _sequenceInner newSequ newMarkings newWeights
