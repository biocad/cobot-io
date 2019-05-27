module Bio.Sequence.Utilities
  ( Range
  , checkRange
  , unsafeEither
  ) where

import           Data.Text (Text)
import qualified Data.Text as T (unpack)

-- | Range of form [a, b).
--
type Range = (Int, Int)

checkRange :: Int -> Range -> Bool
checkRange len (lInd, rInd) = lInd < rInd && 0 <= lInd && rInd <= len

unsafeEither :: Either Text a -> a
unsafeEither = either (error . T.unpack) id
