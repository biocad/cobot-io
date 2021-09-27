module Bio.Sequence.Utilities
  ( unsafeEither
  ) where

import           Data.Text (Text)
import qualified Data.Text as T (unpack)

unsafeEither :: Either Text a -> a
unsafeEither = either (error . T.unpack) id
