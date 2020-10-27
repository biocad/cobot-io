module Bio.FASTA.Parser
  ( fastaP
  , fastaPGeneric
  ) where

import Bio.FASTA.Type       (Fasta, FastaItem (..))
import Bio.Sequence         (BareSequence, bareSequence)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, many', many1', satisfy, space,
                             takeWhile)
import Data.Char            (isLetter)
import Data.Text            (Text, strip)
import Prelude              hiding (takeWhile)

-- | Parser of .fasta file.
--
fastaP :: Parser (Fasta Char)
fastaP = fastaPGeneric isLetter

fastaPGeneric :: (Char -> Bool) -> Parser (Fasta Char)
fastaPGeneric = many' . item

item :: (Char -> Bool) -> Parser (FastaItem Char)
item predicate = FastaItem <$> seqName <*> fastaSeq predicate

seqName :: Parser Text
seqName = strip <$> (char '>' *> tabs *> takeWhile (`notElem` ['\n', '\r']) <* tabs <* eol)

fastaSeq :: (Char -> Bool) -> Parser (BareSequence Char)
fastaSeq predicate = bareSequence . mconcat <$> many' (line predicate)

line :: (Char -> Bool) -> Parser String
line predicate = concat <$> many1' (many1' (satisfy predicate) <* many' (char ' ')) <* eol

eol :: Parser ()
eol = tabs *> choice [slashN, endOfInput]

slashN :: Parser ()
slashN = () <$ many1' space

tabs :: Parser ()
tabs = () <$ many' (char '\t')

