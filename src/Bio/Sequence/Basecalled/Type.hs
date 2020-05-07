module Bio.Sequence.Basecalled.Type
  ( BasecalledSequence (..)
  , BasecalledSequenceWithRawData (..)
  ) where

import           Bio.Sequence (IsSequence (..), WeightedSequence)
import           Data.Coerce  (coerce)
import           Data.Int     (Int16 (..))
import           Data.Vector  (Vector)

newtype BasecalledSequence = BasecalledSequence (WeightedSequence Double Char)
  deriving (Eq, Show)

data BasecalledSequenceWithRawData
  = BasecalledSequenceWithRawData
      { bsSequence      :: BasecalledSequence
        -- A sequence with quality.
      , bsRawG          :: Vector Int16
      , bsRawA          :: Vector Int16
      , bsRawT          :: Vector Int16
      , bsRawC          :: Vector Int16
      , bsPeakLocations :: Vector Int
        -- ^ Same length as 'bsSequence'.
        --
        -- For every base in 'bsSequence' corresponding element in 'bsPeakLocations' is
        -- an index in one of the raw vectors.
      }
  deriving (Eq, Show)

instance IsSequence BasecalledSequence where
  type Element BasecalledSequence = Char
  type Marking BasecalledSequence = ()
  type Weight BasecalledSequence  = Double

  toSequence = coerce
  fromSequence = coerce
