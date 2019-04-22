{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Bio.Sequence
  ( SequenceDecodable (..)

  , Sequence, WeightedSequence, MarkedSequence, BareSequence
  , sequ, markings, weights

  , Marking (..), Weight (..)

  , IsSequence (..), IsWeightedSequence, IsMarkedSequence, IsBareSequence

  , createSequence, unsafeCreateSequence, createBareSequence
  , take, drop, tail, length, reverse, toList, (!), (!?)
  , getRange, unsafeGetRange

  , mean, meanInRange, getWeight, unsafeGetWeight, getWeights, toWeighted, unsafeToWeighted
  , weightedSequence, unsafeWeightedSequence

  , getMarking, unsafeGetMarking, toMarked, unsafeToMarked, addMarkings, unsafeAddMarkings
  , markedSequence, unsafeMarkedSequence
  ) where

import           Control.DeepSeq      (NFData (..))
import           Control.Lens         (makeLenses)
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import           GHC.Generics         (Generic)

import           Control.Lens
import           Control.Monad.Except (MonadError, catchError, throwError)
import           Data.Bifunctor       (bimap, first)
import           Data.Coerce          (Coercible (..))
import           Data.Function        (on)
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as IM (elems, fromList, lookup, mapKeys,
                                             toList, (!))
import           Data.Kind            (Constraint)
import           Data.List            (groupBy, sortOn)
import qualified Data.List            as L (drop, find, head, length, null,
                                            reverse, take)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE (fromList)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M (fromList, notMember, toList, (!))
import           Data.Maybe           (isJust)
import qualified Data.Text            as T (unpack)
import           Data.Type.Equality
import qualified Data.Vector          as V (drop, filter, fromList, length,
                                            reverse, take, toList, (!))
import           GHC.TypeLits         (ErrorMessage (..), TypeError)
import           Prelude              hiding (drop, head, length, null, reverse,
                                       tail, take, (!!))

-- | Class that allows to decode something into Sequence representation
--
class IsSequence s => SequenceDecodable a s where
  sequenceDecode :: a -> Either Text s

-- | Range of form [a, b).
--
type Range = (Int, Int)

-- | Range of form [a, b].
--
type RangeInclusive = (Int, Int)

-- | Type class that descriubes object that is isomorphisc to 'Sequence'
-- and contains elements of type 'Element' s, is marked with marking of type
-- 'Marking'' s and is weighted with weights of type 'Weight'' s.
--
class (Marking (Marking' s), Weight (Weight' s)) => IsSequence s where
  type Element  s :: *
  type Marking' s :: *
  type Weight'  s :: *

  toSequence :: s -> Sequence (Marking' s) (Weight' s) (Element s)
  fromSequence :: Sequence (Marking' s) (Weight' s) (Element s) -> s


-- 'Sequence' represents sequence of objects of type 'a' that
-- can have different markings of type 'b' and weights of type 'w'.
--
data Sequence mk w a = Sequence { _sequ     :: Vector a      -- ^ sequence itself
                                , _markings :: [(mk, Range)] -- ^ list of pairs containing 'Marking' and 'Range', that corresponds to it
                                , _weights  :: Vector w      -- ^ weights for all elements in sequence
                                }
  deriving (Eq, Show)

instance (Marking mk, Weight w) => Semigroup (Sequence mk w a) where
  sequA <> sequB = res
    where
      newSequ     = sequA ^. sequ     <> sequB ^. sequ
      newMarkings = sequA ^. markings <> fmap (fmap (bimap addInd addInd)) (sequB ^. markings)
      newWeights  = sequA ^. weights  <> sequB ^. weights

      res = Sequence newSequ newMarkings newWeights

      addInd :: Int -> Int
      addInd = (+ length sequA)

instance (Marking mk, Weight w) => Monoid (Sequence mk w a) where
  mempty = Sequence mempty mempty mempty

instance (Marking mk, Weight w) => IsSequence (Sequence mk w a) where
  type Element  (Sequence mk w a) = a
  type Marking' (Sequence mk w a) = mk
  type Weight'  (Sequence mk w a) = w

  toSequence = id
  fromSequence = id

--------------------------------------------------------------------------------
-- Getters for 'Sequence'.
--
-- We export only them so user couldn't ruin 'Sequence's invariant.
--------------------------------------------------------------------------------

sequ :: Getter (Sequence mk w a) (Vector a)
sequ = to _sequ

markings :: Getter (Sequence mk w a) [(mk, Range)]
markings = to _markings

weights :: Getter (Sequence mk w a) (Vector w)
weights  = to _weights

--------------------------------------------------------------------------------

-- | Type alias for sequence that has no marking but is weighted.
--
type WeightedSequence w a = Sequence () w a

-- | Type alias for sequence that is not weighted, but has markings.
--
type MarkedSequence mk a  = Sequence mk () a

-- | Type alias for sequence that is not weighted and has no markings.
--
type BareSequence a       = Sequence () () a

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has no markings, but is weighted.
--
type IsWeightedSequence s = (IsSequence s, Unit (Marking' s), NotUnit (Weight' s))

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has markings, but is not weighted.
--
type IsMarkedSequence s   = (IsSequence s, NotUnit (Marking' s), Unit (Weight' s))

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has no markings and is not weighted.
--
type IsBareSequence s     = (IsSequence s, Unit (Marking' s), Unit (Weight' s))

-- | Class that reprsents objects that can be markings of 'Sequence'.
--
class (Eq mk, Ord mk) => Marking mk where

instance Marking ()

-- | Function that retreives all elements in 'IsSequence' @s@ that are covered by given 'Marking'' @s@.
-- Returns 'NonEmpty' list, because if 'Marking' is present in @s@, then list of
-- all 'Marking's for @s@ can't be empty. If given 'Marking' is not found in @s@, an
-- error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > getMarking sequ "Letter A" == ['a', 'a'] :| [['a']]
--
getMarking :: (IsSequence s, NotUnit (Marking' s), MonadError Text m) => s -> Marking' s -> m (NonEmpty [Element s])
getMarking (toSequence -> s) mk | not $ mk `member` (s ^. markings) = throwError markingNotFoundError
                                | otherwise                         = pure $ res
  where
    res = foldl1 (<>) $ fmap ((:| []) . unsafeGetRange s) $ (s ^. markings) `lookupAll` mk

    markingNotFoundError :: Text
    markingNotFoundError = "Bio.Sequence: given marking not found in Sequence."

unsafeGetMarking :: (IsSequence s, NotUnit (Marking' s)) => s -> Marking' s -> NonEmpty [Element s]
unsafeGetMarking mk = unsafeEither . getMarking mk

-- | Converts 'IsBareSequence' @s@ to 'IsMarkedSequence' @s'@ that is marked using provided list
-- of 'Marking's. If at least one of ranges in given list of 'Marking's is out of
-- bounds, an error will be thrown.
--
-- > sequBare = Sequence ['a', 'a', 'b', 'a'] mempty mempty :: BareSequence Char
-- > toMarked sequ [("Letter A", (0, 2)), ("Letter A", (3, 4))] :: MarkedSequence String Char
--
toMarked :: (IsBareSequence s, IsMarkedSequence s', Marking' s' ~ mk, Element s ~ Element s', MonadError Text m) => s -> [(mk, Range)] -> m s'
toMarked (toSequence -> s) = markedSequence (V.toList $ s ^. sequ)

unsafeToMarked :: (IsBareSequence s, IsMarkedSequence s', Marking' s' ~ mk, Element s ~ Element s') => s -> [(mk, Range)] -> s'
unsafeToMarked (toSequence -> s) = unsafeMarkedSequence (V.toList $ s ^. sequ)

-- | Adds new 'Marking's to given 'IsSequence' @s@. Type of new 'Marking's must
-- match type of 'Marking's that @s@ is already marked with. If at least one of ranges
-- in given list of 'Marking's is out of bounds, an error will be thrown.
--
-- > sequ = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4)), ("Letter B", (2, 3))] mempty
-- > sequ' = Sequence ['a', 'a', 'b', 'a'] [("Letter A", (0, 2)), ("Letter A", (3, 4))] mempty
-- > addMarkings sequ' [("Letter B", (2, 3))] == sequ
--
addMarkings :: (IsSequence s, Marking' s ~ mk, MonadError Text m) => s -> [(mk, Range)] -> m s
addMarkings (toSequence -> s) markings' | all (checkRange (length s) . snd) markings' = pure res
                                        | otherwise                                   = throwError rangesError
  where
    res = fromSequence $ Sequence (s ^. sequ) (s ^. markings <> markings') (s ^. weights)

    rangesError :: Text
    rangesError = "Bio.Sequence: can't add markings to Sequence, because some of them are out of range."

unsafeAddMarkings :: (IsSequence s, Marking' s ~ mk) => s -> [(mk, Range)] -> s
unsafeAddMarkings s = unsafeEither . addMarkings s

-- | Create 'IsMarkedSequence' @s@ from list of 'Element's and 'Marking's that
-- cover it. If at least one of ranges in given list of 'Marking's is out of bounds,
-- an error will be thrown.
--
markedSequence :: (IsMarkedSequence s, MonadError Text m) => [Element s] -> [(Marking' s, Range)] -> m s
markedSequence s markings' | L.null markings' = throwError nullMarkingsError
                           | otherwise        = createSequenceInner s markings' []
  where
    nullMarkingsError :: Text
    nullMarkingsError = "Bio.Sequence: can't create marked sequence with null markings."

unsafeMarkedSequence :: IsMarkedSequence s => [Element s] -> [(Marking' s, Range)] -> s
unsafeMarkedSequence s = unsafeEither . markedSequence s

class Weight w where
  toDouble :: w -> Double

instance Weight () where
  toDouble = error "Bio.Sequence: () can't be valid 'Weight'."

instance Weight Double where
  toDouble = id

mean :: (IsSequence s, NotUnit (Weight' s)) => s -> Double
mean s = meanInRange s (0, length s - 1)

meanInRange :: (IsSequence s, NotUnit (Weight' s)) => s -> RangeInclusive -> Double
meanInRange (toSequence -> Sequence{..}) (lInd, rInd) = res
  where
    neededWeights = V.take (rInd + 1) $ V.drop lInd _weights

    res = mean' $ fmap toDouble neededWeights

    mean' :: Vector Double -> Double
    mean' l = sum l / fromIntegral (V.length l)

getWeight :: (IsSequence s, NotUnit (Weight' s), MonadError Text m) => s -> Int -> m (Weight' s)
getWeight (toSequence -> Sequence{..}) i | i >= V.length _weights = throwError indexError
                                         | otherwise              = pure $ _weights V.! i
  where
    indexError :: Text
    indexError = "Bio.Seqence: index out of Sequence's getWeight."

unsafeGetWeight :: (IsSequence s, NotUnit (Weight' s)) => s -> Int -> Weight' s
unsafeGetWeight sequ = either (error. T.unpack) id . getWeight sequ

getWeights :: (IsSequence s, NotUnit w, Weight' s ~ w) => s -> [w]
getWeights = V.toList . _weights . toSequence

toWeighted :: (IsBareSequence s, IsWeightedSequence s', Weight' s' ~ w, Element s ~ Element s', MonadError Text m) => s -> [w] -> m s'
toWeighted (toSequence -> Sequence{..}) = weightedSequence (V.toList _sequ)

unsafeToWeighted :: (IsBareSequence s, IsWeightedSequence s', Weight' s' ~ w, Element s ~ Element s') => s -> [w] -> s'
unsafeToWeighted (toSequence -> Sequence{..}) = unsafeWeightedSequence (V.toList _sequ)

weightedSequence :: (IsWeightedSequence s, MonadError Text m) => [Element s] -> [Weight' s] -> m s
weightedSequence s weights = createSequenceInner s [] weights

unsafeWeightedSequence :: IsWeightedSequence s => [Element s] -> [Weight' s] -> s
unsafeWeightedSequence s = unsafeEither . weightedSequence s

--------------------------------------------------------------------------------
-- Useful functions.
--------------------------------------------------------------------------------

createSequence :: (IsSequence s, NotUnit (Weight' s), NotUnit (Marking' s), MonadError Text m) => [Element s] -> [(Marking' s, Range)] -> [Weight' s] -> m s
createSequence = createSequenceInner

unsafeCreateSequence :: (IsSequence s, NotUnit (Weight' s), NotUnit (Marking' s)) => [Element s] -> [(Marking' s, Range)] -> [Weight' s] -> s
unsafeCreateSequence s markings = unsafeEither . createSequence s markings

createBareSequence :: IsBareSequence s => [Element s] -> s
createBareSequence s = fromSequence $ Sequence (V.fromList s) mempty mempty

toList :: IsSequence s => s -> [Element s]
toList = V.toList . _sequ . toSequence

length :: IsSequence s => s -> Int
length = L.length . _sequ . toSequence

null :: IsSequence s => s -> Bool
null = (== 0) . length

reverse :: IsSequence s => s -> s
reverse (toSequence -> s@Sequence{..}) = res
  where
    newMaxInd = length s - 1

    newSequ     = V.reverse _sequ
    newMarkings = fmap (fmap $ bimap ((-) newMaxInd) ((-) newMaxInd)) _markings
    newWeights  = V.reverse _weights

    res = fromSequence $ Sequence newSequ newMarkings newWeights


drop :: IsWeightedSequence s => Int -> s -> s
drop n (toSequence -> s@Sequence{..}) | n < 0             = error "Bio.Sequence: drop with negative value."
                                      | L.null newWeights = error "Bio.Sequence: no weights left as result of drop of 'WeightedSequence."
                                      | n >= length s     = fromSequence mempty
                                      | otherwise         = res
  where
    droppedSequ = V.drop n _sequ
    newWeights  = V.drop n _weights

    res = fromSequence $ Sequence droppedSequ mempty newWeights

take :: IsWeightedSequence s => Int -> s -> s
take n (toSequence -> Sequence{..}) | n < 0             = error "Bio.Sequence: take with negative value."
                                    | L.null newWeights = error "Bio.Sequence: no weights left as result of take of 'WeightedSequence."
                                    | n == 0            = fromSequence mempty
                                    | otherwise         = res
  where
    takenSequ  = V.take n _sequ
    newWeights = V.take n _weights

    res = fromSequence $ Sequence takenSequ mempty newWeights

tail :: IsWeightedSequence s => s -> s
tail = drop 1

getRange :: (IsSequence s, MonadError Text m) => s -> Range -> m [Element s]
getRange s r@(lInd, rInd) | checkRange (length s) r = pure $ L.drop lInd $ L.take (rInd - lInd) $ toList s
                          | otherwise               = throwError "Bio.Sequence: invalid range in getRange."

unsafeGetRange :: IsSequence s => s -> Range -> [Element s]
unsafeGetRange s = unsafeEither . getRange s

infixl 9 !
(!) :: IsSequence s => s -> Int -> Element s
(!) (toSequence -> Sequence{..}) = (_sequ V.!)

infixl 9 !?
(!?) :: (IsSequence s, MonadError Text m) => s -> Int -> m (Element s)
(!?) s i | i >= length s = throwError "Bio.Sequence: index out of Sequence's length."
         | otherwise     = pure $ s ! i

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

type family NotUnit a :: Constraint where
  NotUnit ()       = TypeError ('Text "Bio.Sequence: this function doesn't work with Sequence that is parametrized by ().")
  NotUnit _        = ()

type family Unit a :: Constraint where
  Unit ()       = ()
  Unit _        = TypeError ('Text "Bio.Sequence: this function doesn't work with Sequence that is not parametrized by ().")

member :: Eq a => a -> [(a, b)] -> Bool
member a l = isJust . L.find ((== a) . fst) $ l

lookupAll :: Eq a => [(a, b)] -> a -> [b]
lookupAll [] _                         = []
lookupAll ((a, b) : xs) a' | a == a'   = b : lookupAll xs a'
                           | otherwise = lookupAll xs a'

unsafeEither :: Either Text a -> a
unsafeEither = either (error . T.unpack) id

createSequenceInner :: (IsSequence s, MonadError Text m) => [Element s] -> [(Marking' s, Range)] -> [Weight' s] -> m s
createSequenceInner s markings weights | not checkRanges  = throwError rangesError
                                       | not checkWeights = throwError weightsError
                                       | otherwise        = pure resSequence
  where
    markingsGrouped = groupBy ((==) `on` fst) . sortOn fst $ markings

    seqVector     = V.fromList s
    weightsVector = V.fromList weights

    resSequence = fromSequence $ Sequence seqVector markings weightsVector

    checkRanges :: Bool
    checkRanges = all (checkRange (L.length s)) $ fmap snd markings

    checkWeights :: Bool
    checkWeights = L.length s == L.length weights

    rangesError :: Text
    rangesError = "Bio.Sequence: invalid 'Range' found in sequence's marking."

    weightsError :: Text
    weightsError = "Bio.Sequence: sequence and weights have different lengths."

checkRange :: Int -> Range -> Bool
checkRange len (lInd, rInd) = lInd < rInd && 0 <= lInd && rInd <= len

