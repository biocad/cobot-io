{-# LANGUAGE ViewPatterns #-}

module Bio.Sequence.Functions.Marking
  ( getMarking
  , unsafeGetMarking
  , toMarked
  , unsafeToMarked
  , addMarkings
  , unsafeAddMarkings
  , listMarkings
  ) where

import           Bio.Sequence.Class              (ContainsMarking,
                                                  IsBareSequence,
                                                  IsMarkedSequence,
                                                  IsSequence (..),
                                                  markedSequence, markings,
                                                  sequ, unsafeMarkedSequence,
                                                  weights, _sequenceInner)
import           Bio.Sequence.Functions.Sequence (length, unsafeGetRange)
import           Bio.Sequence.Utilities          (Range, checkRange,
                                                  unsafeEither)
import           Control.Lens
import           Control.Monad.Except            (MonadError, throwError)
import           Data.List                       (nub)
import           Data.List.NonEmpty              (NonEmpty (..))
import           Data.Text                       (Text)
import qualified Data.Vector                     as V (toList)
import           Prelude                         hiding (drop, head, length,
                                                  null, reverse, tail, take,
                                                  (!!))

-- | Function that retrieves all elements in 'IsSequence' @s@ that are covered by given 'Marking'' @s@.
-- Returns 'NonEmpty' list, because if 'Marking' is present in @s@, then list of
-- all 'Marking's for @s@ can't be empty. If given 'Marking is not found in @s@, an
-- error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > getMarking sequ "Letter A" == ['a', 'a'] :| [['a']]
--
getMarking :: (ContainsMarking s, MonadError Text m) => s -> Marking s -> m (NonEmpty [Element s])
getMarking (toSequence -> s) mk | not $ mk `member` (s ^. markings) = throwError markingNotFoundError
                                | otherwise                         = pure $ res
  where
    res = foldl1 (<>) $ fmap ((:| []) . unsafeGetRange s) $  mk `lookupAll` (s ^. markings)

    markingNotFoundError :: Text
    markingNotFoundError = "Bio.Sequence.Functions.Marking: given marking not found in Sequence."

unsafeGetMarking :: ContainsMarking s => s -> Marking s -> NonEmpty [Element s]
unsafeGetMarking mk = unsafeEither . getMarking mk

-- | Converts 'IsBareSequence' @s@ to 'IsMarkedSequence' @s'@ that is marked using provided list
-- of 'Marking's. If at least one of ranges in given list of 'Marking's is out of
-- bounds, an error will be thrown.
--
-- > sequBare = Sequence ['a', 'a', 'b', 'a'] mempty mempty :: BareSequence Char
-- > toMarked sequ [("Letter A", (0, 2)), ("Letter A", (3, 4))] :: MarkedSequence String Char
--
toMarked :: (IsBareSequence s, IsMarkedSequence s', Marking s' ~ mk, Element s ~ Element s', MonadError Text m) => s -> [(mk, Range)] -> m s'
toMarked (toSequence -> s) = markedSequence (V.toList $ s ^. sequ)

unsafeToMarked :: (IsBareSequence s, IsMarkedSequence s', Marking s' ~ mk, Element s ~ Element s') => s -> [(mk, Range)] -> s'
unsafeToMarked (toSequence -> s) = unsafeMarkedSequence (V.toList $ s ^. sequ)

-- | Adds new 'Marking's to given 'IsSequence' @s@. Type of new 'Marking's must
-- match type of 'Marking's that @s@ is already marked with. If at least one of ranges
-- in given list of 'Marking's is out of bounds, an error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > sequ' = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4))] mempty
-- > addMarkings sequ' [("Letter B", (2, 3))] == sequ
--
addMarkings :: (ContainsMarking s, Marking s ~ mk, MonadError Text m) => s -> [(mk, Range)] -> m s
addMarkings (toSequence -> s) markings' | all (checkRange (length s) . snd) markings' = pure res
                                        | otherwise                                   = throwError rangesError
  where
    res = fromSequence $ _sequenceInner (s ^. sequ) (s ^. markings <> markings') (s ^. weights)

    rangesError :: Text
    rangesError = "Bio.Sequence.Functions.Marking: can't add markings to Sequence, because some of them are out of range."

unsafeAddMarkings :: (ContainsMarking s, Marking s ~ mk) => s -> [(mk, Range)] -> s
unsafeAddMarkings s = unsafeEither . addMarkings s

-- | Retrieves all 'Marking's from given sequence that 'ContainsMarking'.
--   Result is list of 'Marking's without dublicates.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > listMarkings sequ == ["Letter A", "Letter B"]
--
listMarkings :: ContainsMarking s => s -> [Marking s]
listMarkings (toSequence -> s) = nub $ fst <$> s ^. markings

--------------------------------------------------------------------------------
-- Inner functions.
--------------------------------------------------------------------------------

member :: Eq a => a -> [(a, b)] -> Bool
member a = (a `elem`) . fmap fst

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a = fmap snd . filter ((== a) . fst)
