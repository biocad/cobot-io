module Bio.Sequence.Basecalled.Type
  ( BasecalledSequence (..)
  ) where

import           Bio.Sequence (IsSequence (..), WeightedSequence)
import           Data.Coerce  (coerce)

newtype BasecalledSequence = BasecalledSequence (WeightedSequence Double Char)
  deriving (Eq, Show)

instance IsSequence BasecalledSequence where
  type Element BasecalledSequence = Char
  type Marking BasecalledSequence = ()
  type Weight BasecalledSequence  = Double

  toSequence = coerce
  fromSequence = coerce

