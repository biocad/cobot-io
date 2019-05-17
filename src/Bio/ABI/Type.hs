module Bio.ABI.Type
  ( ABIProcessed (..)
  ) where

import           Bio.Sequence (IsSequence (..), WeightedSequence)
import           Data.Coerce  (coerce)

newtype ABIProcessed = ABIProcessed (WeightedSequence Double Char)
  deriving (Eq, Show)

instance IsSequence ABIProcessed where
  type Element ABIProcessed = Char
  type Marking ABIProcessed = ()
  type Weight ABIProcessed  = Double

  toSequence = coerce
  fromSequence = coerce

