{-# LANGUAGE UndecidableInstances #-}

module Bio.Sequence
  ( module Bio.Sequence.Class
  , module Bio.Sequence.Functions.Sequence
  , module Bio.Sequence.Functions.Weight
  , module Bio.Sequence.Functions.Marking
  , module Bio.Sequence.Range
  ) where

import Bio.Sequence.Class              hiding (_sequenceInner)
import Bio.Sequence.Functions.Marking
import Bio.Sequence.Functions.Sequence
import Bio.Sequence.Functions.Weight
import Bio.Sequence.Range


