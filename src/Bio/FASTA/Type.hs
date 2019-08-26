module Bio.FASTA.Type
  ( Fasta
  , FastaItem (..)
  ) where

import           Bio.Sequence (BareSequence)
import           Data.Text    (Text)

-- | Type alias for FASTA file.
--
type Fasta a = [FastaItem a]

-- | One record in FASTA file.
--
data FastaItem a
   = FastaItem { name :: Text           -- ^ name of the sequence
               , sequ :: BareSequence a -- ^ bare sequence
               }
  deriving (Eq, Show, Functor)
