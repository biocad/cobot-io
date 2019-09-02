module Bio.FASTA.Parser
  ( fastaP
  ) where

import           Bio.FASTA.Type       (Fasta)
import           Data.Attoparsec.Text (Parser)

-- | Parser of .fasta file.
--
fastaP :: Parser (Fasta Char)
fastaP = undefined
