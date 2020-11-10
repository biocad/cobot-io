{-# LANGUAGE UndecidableInstances #-}

module Bio.Sequence.Class
  (
  -- class for decoding into sequence
    SequenceDecodable (..)

  -- 'Sequence' type itself
  , Sequence
  , WeightedSequence
  , MarkedSequence
  , BareSequence
  , Range
  , sequ
  , markings
  , weights
  , bareSequ

  -- classes for weights and markings of sequence
  , IsMarking
  , IsWeight (..)

  -- classes that are abstractions over 'Sequence'
  , IsSequence (..)
  , IsWeightedSequence
  , IsMarkedSequence
  , IsBareSequence
  , ContainsMarking
  , ContainsNoMarking
  , ContainsWeight
  , ContainsNoWeight

  -- constructors for 'IsSequence'
  , createSequence
  , unsafeCreateSequence
  , bareSequence
  , weightedSequence
  , unsafeWeightedSequence
  , markedSequence
  , unsafeMarkedSequence

  -- inner unsafe constructor that should be used only in module Bio.Sequence
  , _sequenceInner
  ) where

import           Bio.Sequence.Utilities (Range, checkRange, unsafeEither)
import           Control.Lens
import           Control.Monad.Except   (MonadError, throwError)
import           Data.Kind              (Constraint)
import qualified Data.List              as L (length, null)
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V (fromList, length)
import           GHC.Generics           (Generic)
import           GHC.TypeLits           (ErrorMessage (..), TypeError)

--------------------------------------------------------------------------------
-- Sequence datatype.
--------------------------------------------------------------------------------

-- 'Sequence' represents sequence of objects of type 'a' that
-- can have different markings of type 'mk' and weights of type 'w'.
--
data Sequence mk w a = Sequence { _sequ     :: Vector a      -- ^ sequence itself
                                , _markings :: [(mk, Range)] -- ^ list of pairs containing marking and 'Range', that corresponds to it
                                , _weights  :: Vector w      -- ^ weights for all elements in sequence
                                }
  deriving (Eq, Show, Generic, Functor)

instance Semigroup (Sequence mk w a) where
  sequA <> sequB = res
    where
      newSequ     = sequA ^. sequ     <> sequB ^. sequ
      newMarkings = sequA ^. markings <> fmap (fmap (bimap addInd addInd)) (sequB ^. markings)
      newWeights  = sequA ^. weights  <> sequB ^. weights

      res = Sequence newSequ newMarkings newWeights

      addInd :: Int -> Int
      addInd = (+ V.length (sequA ^. sequ))

instance Monoid (Sequence mk () a) where
  mempty = Sequence mempty mempty mempty

instance Foldable (Sequence mk w) where
  foldMap f = foldMap f . _sequ

  length = V.length . _sequ

instance Traversable (Sequence mk w) where
  traverse f s@Sequence{..} = (\newSeq -> s { _sequ = newSeq }) <$> traverse f _sequ

-- | Exported constructor for 'Sequence'. Should be used ONLY in module Bio.Sequence.
--
_sequenceInner :: Vector a -> [(mk, Range)] -> Vector w -> Sequence mk w a
_sequenceInner = Sequence

--------------------------------------------------------------------------------
-- Aliases for 'Sequence'.
--------------------------------------------------------------------------------

-- | Type alias for sequence that has no marking but is weighted.
--
type WeightedSequence w a = Sequence () w a

-- | Type alias for sequence that is not weighted, but has markings.
--
type MarkedSequence mk a = Sequence mk () a

-- | Type alias for sequence that is not weighted and has no markings.
--
type BareSequence a = Sequence () () a

--------------------------------------------------------------------------------
-- Lenses for 'Sequence'.
-- We create only getters, so user that couldn't ruin 'Sequence's invariant.
-- But we can create a Lens for 'BareSequence', and it won't ruin any invariants.
--------------------------------------------------------------------------------

sequ :: Getter (Sequence mk w a) (Vector a)
sequ = to _sequ

markings :: Getter (Sequence mk w a) [(mk, Range)]
markings = to _markings

weights :: Getter (Sequence mk w a) (Vector w)
weights  = to _weights

bareSequ :: Lens' (BareSequence a) (Vector a)
bareSequ = lens _sequ (\s v -> s { _sequ = v })


--------------------------------------------------------------------------------
-- IsMarking class.
--------------------------------------------------------------------------------

-- | Class that reprsents objects that can be markings of 'IsSequence'.
--
class (Eq mk, Ord mk) => IsMarking mk where

instance IsMarking ()


--------------------------------------------------------------------------------
-- IsWeight class.
--------------------------------------------------------------------------------

-- | Class that represents objects that can be weights of 'IsSequence's 'Element's.
--
class IsWeight w where
  toDouble :: w -> Double

instance IsWeight () where
  toDouble = error "Bio.Sequence.Class: () can't be valid 'Weight'."

instance IsWeight Double where
  toDouble = id


--------------------------------------------------------------------------------
-- IsSequence class.
--------------------------------------------------------------------------------

-- | Type class that describes object that is isomorphisc to 'Sequence'
-- and contains elements of type 'Element', is marked with marking of type
-- 'Marking' and is weighted with weights of type 'Weight'.
--
-- Special cases, when 'IsSequence' has no markings, has no weights, or
-- has no weights and no markings at the same time are aliased:
--  * 'IsWeightedSequence' is alias for sequence with 'Marking' type set to () and 'Weight' not set to ()
--  * 'IsMarkedSequence' is alias for sequence with 'Weight' type set to () and 'Marking' not set to ().
--  * 'IsBareSequence' is alias for sequence with 'Marking' and 'Weight' types
--     set to ().
--
-- Instances of 'IsSequence' can be built only using special constructors:
--  * 'createSequence' creates 'IsSequence' that has 'Marking' and 'Weight'
--    that are not set to ().
--  * 'bareSequence' creates 'IsSequence' that has 'Marking' and 'Weight'
--    that are set to ().
--  * 'weightedSequence' creates 'IsSequence' that has 'Marking' set to () and
--    'Weight' that is not ().
--  * 'markedSequence' creates 'IsSequence' that has 'Weight' set to () and
--    'Marking' that is not ().
--
-- Constraints and constructors mentioned above gaurantee that 'IsSequence'
-- instances that have no 'Weight's will in compile-time
-- have () as types assosiated with their 'Weight's.
-- That is used to make functions that rely on 'IsSequence' instance
-- having not null weights type-safe.
--
class (IsMarking (Marking s), IsWeight (Weight s)) => IsSequence s where
  type Element s :: *
  type Marking s :: *
  type Weight  s :: *

  toSequence :: s -> Sequence (Marking s) (Weight s) (Element s)
  fromSequence :: Sequence (Marking s) (Weight s) (Element s) -> s

instance (IsMarking mk, IsWeight w) => IsSequence (Sequence mk w a) where
  type Element (Sequence mk w a) = a
  type Marking (Sequence mk w a) = mk
  type Weight  (Sequence mk w a) = w

  toSequence = id
  fromSequence = id

-- | Class that allows to decode something into Sequence representation
--
class IsSequence s => SequenceDecodable a s where
  sequenceDecode :: a -> Either Text s


--------------------------------------------------------------------------------
-- Aliases for 'IsSequence'.
--------------------------------------------------------------------------------

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has no markings, but is weighted.
--
type IsWeightedSequence s = (IsSequence s, Unit (Marking s), NotUnit (Weight s))

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has markings, but is not weighted.
--
type IsMarkedSequence s = (IsSequence s, NotUnit (Marking s), Unit (Weight s))

-- | Type alias for constraint that checks whether given instance of 'IsSequence'
-- has no markings and is not weighted.
--
type IsBareSequence s = (IsSequence s, Unit (Marking s), Unit (Weight s))

-- | Type alias for constraint that checks whether given instance @s@ of 'IsSequence'
-- has markings, weights of @s@ are not checked.
--
type ContainsMarking s = (IsSequence s, NotUnit (Marking s))

-- | Type alias for constraint that checks whether given instance @s@ of 'IsSequence'
-- has no markings, weights of @s@ are not checked.
--
type ContainsNoMarking s = (IsSequence s, Unit (Marking s))

-- | Type alias for constraint that checks whether given instance @s@ of 'IsSequence'
-- has weights, markings of @s@ are not checked.
--
type ContainsWeight s = (IsSequence s, NotUnit (Weight s))

-- | Type alias for constraint that checks whether given instance @s@ of 'IsSequence'
-- has no weights, markings of @s@ are not checked.
--
type ContainsNoWeight s = (IsSequence s, Unit (Weight s))


--------------------------------------------------------------------------------
-- Constructors for 'IsSequence's.
--------------------------------------------------------------------------------

-- | Create 'IsSequence' @s@ that has both markings and weights.
-- If any of the markings is invalid or length of weights list is not equal to length
-- of sequence, an error will be thrown.
--
createSequence :: (ContainsMarking s, ContainsWeight s, MonadError Text m) => [Element s] -> [(Marking s, Range)] -> [Weight s] -> m s
createSequence = createSequenceInner True True

unsafeCreateSequence :: (ContainsMarking s, ContainsWeight s) => [Element s] -> [(Marking s, Range)] -> [Weight s] -> s
unsafeCreateSequence s markings' = unsafeEither . createSequence s markings'

-- | Create 'IsBareSequence' @s@, simple sequence without markings and weights.
--
bareSequence :: IsBareSequence s => [Element s] -> s
bareSequence s = fromSequence $ Sequence (V.fromList s) mempty mempty

-- | Create 'IsMarkedSequence' @s@ from list of 'Element's and 'Marking's that
-- mark it. If at least one of ranges in given list of 'Marking's is out of bounds,
-- an error will be thrown.
--
markedSequence :: (IsMarkedSequence s, MonadError Text m) => [Element s] -> [(Marking s, Range)] -> m s
markedSequence s markings' = createSequenceInner True False s markings' []

unsafeMarkedSequence :: IsMarkedSequence s => [Element s] -> [(Marking s, Range)] -> s
unsafeMarkedSequence s = unsafeEither . markedSequence s

-- | Create 'IsWeightedSequence' @s@ from list of 'Element's and 'Weight's
-- that correspond to each 'Element'. If length of list of 'Weight's
-- is not equal to length of sequence or lis is null, an error will be thrown.
--
weightedSequence :: (IsWeightedSequence s, MonadError Text m) => [Element s] -> [Weight s] -> m s
weightedSequence s = createSequenceInner False True s []

unsafeWeightedSequence :: IsWeightedSequence s => [Element s] -> [Weight s] -> s
unsafeWeightedSequence s = unsafeEither . weightedSequence s

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

type family NotUnit a :: Constraint where
  NotUnit () = TypeError ('Text "cobot-io: this function doesn't work with when parametrized by ().")
  NotUnit _  = ()

type family Unit a :: Constraint where
  Unit () = ()
  Unit _  = TypeError ('Text "cobot-io: this function doesn't work with when not parametrized by ().")

createSequenceInner :: (IsSequence s, MonadError Text m) => Bool -> Bool -> [Element s] -> [(Marking s, Range)] -> [Weight s] -> m s
createSequenceInner checkMk checkW s markings' weights' | checkMk && not checkRanges     = throwError rangesError
                                                        | checkW && not checkNullWeights = throwError weightsNullError
                                                        | checkW && not checkLenWeights  = throwError weightsLenError
                                                        | otherwise                      = pure resSequence
  where
    seqVector     = V.fromList s
    weightsVector = V.fromList weights'

    resSequence = fromSequence $ Sequence seqVector markings' weightsVector

    checkRanges :: Bool
    checkRanges = all (checkRange (L.length s)) $ fmap snd markings'

    checkNullWeights :: Bool
    checkNullWeights = not (L.null weights')

    checkLenWeights :: Bool
    checkLenWeights = L.length s == L.length weights'

    rangesError :: Text
    rangesError = "Bio.Sequence.Class: invalid 'Range' found in sequence's marking."

    weightsNullError :: Text
    weightsNullError = "Bio.Sequence.Class: weights are null for sequence."

    weightsLenError :: Text
    weightsLenError = "Bio.Sequence.Class: sequence and weights have different lengths."
