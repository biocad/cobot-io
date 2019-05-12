module Bio.ABI.Type
  ( ABIRaw (..)
  ) where

import           Bio.Sequence (IsSequence (..), WeightedSequence)
import           Data.Coerce  (coerce)

newtype ABIRaw = ABIRaw (WeightedSequence Double Char)
  deriving (Eq, Show)

instance IsSequence ABIRaw where
  type Element ABIRaw  = Char
  type Marking ABIRaw = ()
  type Weight ABIRaw  = Double

  toSequence = coerce
  fromSequence = coerce

