module Bio.FASTA.Parser
  ( fastaP
  ) where

import           Bio.FASTA.Type       (Fasta, FastaItem(..))
import           Bio.Sequence       (BareSequence, bareSequence)
import           Data.Attoparsec.Text (Parser, many', many1', char, endOfLine, letter,
                                       takeWhile, choice, endOfInput)
import           Prelude              hiding (takeWhile)
import           Data.Text    (Text)

-- | Parser of .fasta file.
--
fastaP :: Parser (Fasta Char)
fastaP = many' item

item :: Parser (FastaItem Char)
item = FastaItem <$> seqName <*> fastaSeq

seqName :: Parser (Text)
seqName = tabs *> char '>' *> takeWhile (`notElem` ['\n', '\r', '\t']) <* eol

fastaSeq :: Parser (BareSequence Char)
fastaSeq = bareSequence . mconcat <$> many1' line

line :: Parser (String)
line = tabs *> many1' letter <* eol

eol :: Parser ()
eol = tabs *> choice [slashN, endOfInput]

slashN :: Parser ()
slashN = () <$ many1' endOfLine

tabs :: Parser ()
tabs = () <$ many' (char '\t')

