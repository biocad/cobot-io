{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.FASTA.Parser
  ( fastaP
  , fastaPGeneric
  , fastaLine
  , modificationP
  ) where

import Bio.FASTA.Type       (Fasta, FastaItem (..), ModItem (..), Modification (..),
                             ParsableFastaToken (..))
import Bio.Sequence         (BareSequence, bareSequence)
import Control.Applicative  ((<|>))
import Data.Char            (isAlphaNum, isLetter)
import Data.Functor         (void, ($>))
import Data.Text            (Text, pack, strip)
import Data.Void            (Void)
import Text.Megaparsec      (Parsec, anySingle, eof, hidden, many, manyTill, satisfy, some, try,
                             (<?>))
import Text.Megaparsec.Char (char, eol, hspace, space, string)


instance ParsableFastaToken Char where
    parseToken = satisfy

instance ParsableFastaToken ModItem where
    parseToken predicate = Mod <$> modificationP <|> Letter <$> satisfy predicate

type Parser = Parsec Void Text

-- | Parser of .fasta file.
--
fastaP :: ParsableFastaToken a => Parser (Fasta a)
fastaP = hidden space *> fastaPGeneric isLetter

fastaPGeneric :: ParsableFastaToken a => (Char -> Bool) -> Parser (Fasta a)
fastaPGeneric = many . item

item :: ParsableFastaToken a => (Char -> Bool) -> Parser (FastaItem a)
item predicate =
  FastaItem
    <$> seqName
    <*> (fastaSeq predicate <?> "sequence")

seqName :: Parser Text
seqName = strip . pack <$> (char '>' *> (manyTill anySingle myEnd <?> "sequence name"))

fastaSeq :: ParsableFastaToken a => (Char -> Bool) -> Parser (BareSequence a)
fastaSeq predicate = bareSequence . concat <$> many (fastaLine predicate) <* hidden space

fastaLine :: ParsableFastaToken a => (Char -> Bool) -> Parser [a]
fastaLine predicate = concat <$> some (some (parseToken predicate <* hidden hspace)) <* myEnd

myEnd :: Parser ()
myEnd = void (some eol) <|> eof

modificationP :: Parser Modification
modificationP
  =   string "[A*]" $> Mod_A_Star
  <|> string "[C*]" $> Mod_C_Star
  <|> string "[G*]" $> Mod_G_Star
  <|> string "[T*]" $> Mod_T_Star
  <|> string "[rA]" $> Mod_rA
  <|> string "[rC]" $> Mod_rC
  <|> string "[rG]" $> Mod_rG
  <|> string "[rU]" $> Mod_rU
  <|> string "[+A]" $> Mod_Plus_A
  <|> string "[+C]" $> Mod_Plus_C
  <|> string "[+G]" $> Mod_Plus_G
  <|> string "[+T]" $> Mod_Plus_T
  <|> string "[rAf]" $> Mod_rAf
  <|> string "[rCf]" $> Mod_rCf
  <|> string "[rGf]" $> Mod_rGf
  <|> string "[rUf]" $> Mod_rUf
  <|> string "[mA]" $> Mod_mA
  <|> string "[mC]" $> Mod_mC
  <|> string "[mG]" $> Mod_mG
  <|> string "[mU]" $> Mod_mU
  <|> string "[mA*]" $> Mod_mA_Star
  <|> string "[mC*]" $> Mod_mC_Star
  <|> string "[mG*]" $> Mod_mG_Star
  <|> string "[mU*]" $> Mod_mU_Star
  <|> string "[dU]" $> Mod_dU
  <|> string "[5Bio]" $> Mod_5Bio
  <|> string "[iBio]" $> Mod_iBio
  <|> string "[56FAM]" $> Mod_56FAM
  <|> string "[36FAM]" $> Mod_36FAM
  <|> string "[5HEX]" $> Mod_5HEX
  <|> string "[5TMR]" $> Mod_5TMR
  <|> string "[3BHQ1]" $> Mod_3BHQ1
  <|> string "[3BHQ2]" $> Mod_3BHQ2
  <|> string "[5NH2]" $> Mod_5NH2
  <|> string "[3NH2]" $> Mod_3NH2
  <|> string "[5PO4]" $> Mod_5PO4
  <|> string "[3PO4]" $> Mod_3PO4
  <|> string "[3BioTEG]" $> Mod_3BioTEG
  <|> string "[C12]" $> Mod_C12
  <|> string "[NHSdT]" $> Mod_NHSdT
  <|> string "[5Mal]" $> Mod_5Mal
  <|> string "[5thio]" $> Mod_5thio
  <|> string "[3thio]" $> Mod_3thio
  <|> string "[3azide]" $> Mod_3azide
  <|> string "[3alkine]" $> Mod_3alkine
  <|> string "[5CholTEG]" $> Mod_5CholTEG
  <|> string "[3CholTEG]" $> Mod_3CholTEG
  <|> string "[5C10]" $> Mod_5C10
  <|> string "[5Alk]" $> Mod_5Alk
  <|> string "[GC]" $> Mod_GC
  <|> string "[GT]" $> Mod_GT
  <|> string "[AT]" $> Mod_AT
  <|> string "[TG]" $> Mod_TG
  <|> string "[AC]" $> Mod_AC
  <|> string "[CC]" $> Mod_CC
  <|> string "[AA]" $> Mod_AA
  <|> string "[TC]" $> Mod_TC
  <|> string "[TT]" $> Mod_TT
  <|> string "[CG]" $> Mod_CG
  <|> string "[GG]" $> Mod_GG
  <|> string "[AG]" $> Mod_AG
  <|> string "[GA]" $> Mod_GA
  <|> string "[CA]" $> Mod_CA
  <|> string "[CT]" $> Mod_CT
  <|> string "[TA]" $> Mod_TA
  <|> string "[AAA]" $> Mod_AAA
  <|> string "[AAC]" $> Mod_AAC
  <|> string "[ACT]" $> Mod_ACT
  <|> string "[ATC]" $> Mod_ATC
  <|> string "[ATG]" $> Mod_ATG
  <|> string "[CAG]" $> Mod_CAG
  <|> string "[AGA]" $> Mod_AGA
  <|> string "[CAT]" $> Mod_CAT
  <|> string "[CCG]" $> Mod_CCG
  <|> string "[CGT]" $> Mod_CGT
  <|> string "[CTG]" $> Mod_CTG
  <|> string "[GAA]" $> Mod_GAA
  <|> string "[GAC]" $> Mod_GAC
  <|> string "[GCT]" $> Mod_GCT
  <|> string "[GGT]" $> Mod_GGT
  <|> string "[GTT]" $> Mod_GTT
  <|> string "[TAC]" $> Mod_TAC
  <|> string "[TCT]" $> Mod_TCT
  <|> string "[TGC]" $> Mod_TGC
  <|> string "[TGG]" $> Mod_TGG
  <|> string "[TTC]" $> Mod_TTC
  <|> string "[TTT]" $> Mod_TTT
  <|> unknownP

unknownP :: Parser Modification
unknownP = try $ do
    _ <- char '['
    m <- some (satisfy (\c -> isAlphaNum c || c `elem` ['+', '-', '*', '_'])) <?> "modification name"
    _ <- char ']'
    pure $ Unknown ("[" <> m <> "]")
