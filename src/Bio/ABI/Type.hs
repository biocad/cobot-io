module Bio.ABI.Type
  ( ABIBasecalled (..)
  ) where

import           Bio.Sequence (IsSequence (..), WeightedSequence)
import           Data.Coerce  (coerce)

newtype ABIBasecalled = ABIBasecalled (WeightedSequence Double Char)
  deriving (Eq, Show)

instance IsSequence ABIBasecalled where
  type Element ABIBasecalled = Char
  type Marking ABIBasecalled = ()
  type Weight ABIBasecalled  = Double

  toSequence = coerce
  fromSequence = coerce

