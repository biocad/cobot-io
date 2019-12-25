module Bio.FASTA.Parser
  ( fastaP
  ) where

import           Bio.FASTA.Type       (Fasta, FastaItem (..))
import           Bio.Sequence         (BareSequence, bareSequence)
import           Data.Attoparsec.Text (Parser, char, choice, endOfInput,
                                       endOfLine, letter, many', many1',
                                       takeWhile)
import           Data.Text            (Text, strip)
import           Prelude              hiding (takeWhile)

-- | Parser of .fasta file.
--
fastaP :: Parser (Fasta Char)
fastaP = many' item

item :: Parser (FastaItem Char)
item = FastaItem <$> seqName <*> fastaSeq

seqName :: Parser (Text)
seqName = strip <$> (char '>' *> tabs *> takeWhile (`notElem` ['\n', '\r']) <* tabs <* eol)

fastaSeq :: Parser (BareSequence Char)
fastaSeq = bareSequence . mconcat <$> many' line

line :: Parser String
line = concat <$> many1' (many1' letter <* many' (char ' ')) <* eol

eol :: Parser ()
eol = tabs *> choice [slashN, endOfInput]

slashN :: Parser ()
slashN = () <$ many1' endOfLine

tabs :: Parser ()
tabs = () <$ many' (char '\t')

