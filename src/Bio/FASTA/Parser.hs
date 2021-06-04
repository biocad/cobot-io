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
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', satisfy,
                             skipWhile, string, takeWhile, try)
import Data.Char            (isAlphaNum, isLetter, isSpace)
import Data.Text            (Text, strip)
import Prelude              hiding (takeWhile)

instance ParsableFastaToken Char where
    parseToken = satisfy

instance ParsableFastaToken ModItem where
    parseToken predicate = (Mod <$> modificationP) <|> (Letter <$> satisfy predicate)


-- | Parser of .fasta file.
--
fastaP :: ParsableFastaToken a => Parser (Fasta a)
fastaP = many' slashN *> fastaPGeneric isLetter

fastaPGeneric :: ParsableFastaToken a => (Char -> Bool) -> Parser (Fasta a)
fastaPGeneric = many' . item

item :: ParsableFastaToken a => (Char -> Bool) -> Parser (FastaItem a)
item predicate = (FastaItem <$> seqName <*> fastaSeq predicate) <* skipWhile isSpace

seqName :: Parser Text
seqName = strip <$> (char '>' *> tabs *> takeWhile (`notElem` ['\n', '\r']) <* tabs <* eol)

fastaSeq :: ParsableFastaToken a => (Char -> Bool) -> Parser (BareSequence a)
fastaSeq predicate = bareSequence . mconcat <$> many' (fastaLine predicate)

fastaLine :: ParsableFastaToken a => (Char -> Bool) -> Parser [a]
fastaLine predicate = concat <$> many1' (many1' (parseToken predicate) <* many' (char ' ')) <* eol

eol :: Parser ()
eol = tabs *> choice [slashN, endOfInput]

slashN :: Parser ()
slashN = () <$ many1' endOfLine

tabs :: Parser ()
tabs = () <$ many' (char '\t')

modificationP :: Parser Modification
modificationP 
  =   string "[A*]" *> pure Mod_A_Star
  <|> string "[C*]" *> pure Mod_C_Star
  <|> string "[G*]" *> pure Mod_G_Star
  <|> string "[T*]" *> pure Mod_T_Star
  <|> string "[rA]" *> pure Mod_rA
  <|> string "[rC]" *> pure Mod_rC
  <|> string "[rG]" *> pure Mod_rG
  <|> string "[rU]" *> pure Mod_rU
  <|> string "[+A]" *> pure Mod_Plus_A
  <|> string "[+C]" *> pure Mod_Plus_C
  <|> string "[+G]" *> pure Mod_Plus_G
  <|> string "[+T]" *> pure Mod_Plus_T
  <|> string "[rAf]" *> pure Mod_rAf
  <|> string "[rCf]" *> pure Mod_rCf
  <|> string "[rGf]" *> pure Mod_rGf
  <|> string "[rUf]" *> pure Mod_rUf
  <|> string "[mA]" *> pure Mod_mA
  <|> string "[mC]" *> pure Mod_mC
  <|> string "[mG]" *> pure Mod_mG
  <|> string "[mU]" *> pure Mod_mU
  <|> string "[mA*]" *> pure Mod_mA_Star
  <|> string "[mC*]" *> pure Mod_mC_Star
  <|> string "[mG*]" *> pure Mod_mG_Star
  <|> string "[mU*]" *> pure Mod_mU_Star
  <|> string "[dU]" *> pure Mod_dU
  <|> string "[5Bio]" *> pure Mod_5Bio
  <|> string "[iBio]" *> pure Mod_iBio
  <|> string "[56FAM]" *> pure Mod_56FAM
  <|> string "[36FAM]" *> pure Mod_36FAM
  <|> string "[5HEX]" *> pure Mod_5HEX
  <|> string "[5TMR]" *> pure Mod_5TMR
  <|> string "[3BHQ1]" *> pure Mod_3BHQ1
  <|> string "[3BHQ2]" *> pure Mod_3BHQ2
  <|> string "[5NH2]" *> pure Mod_5NH2
  <|> string "[3NH2]" *> pure Mod_3NH2
  <|> string "[5PO4]" *> pure Mod_5PO4
  <|> string "[3PO4]" *> pure Mod_3PO4
  <|> string "[3BioTEG]" *> pure Mod_3BioTEG
  <|> string "[C12]" *> pure Mod_C12
  <|> string "[NHSdT]" *> pure Mod_NHSdT
  <|> string "[5Mal]" *> pure Mod_5Mal
  <|> string "[5thio]" *> pure Mod_5thio
  <|> string "[3thio]" *> pure Mod_3thio
  <|> string "[3azide]" *> pure Mod_3azide
  <|> string "[3alkine]" *> pure Mod_3alkine
  <|> string "[5CholTEG]" *> pure Mod_5CholTEG
  <|> string "[3CholTEG]" *> pure Mod_3CholTEG
  <|> string "[5C10]" *> pure Mod_5C10
  <|> string "[5Alk]" *> pure Mod_5Alk
  <|> string "[GC]" *> pure Mod_GC
  <|> string "[GT]" *> pure Mod_GT
  <|> string "[AT]" *> pure Mod_AT
  <|> string "[TG]" *> pure Mod_TG
  <|> string "[AC]" *> pure Mod_AC
  <|> string "[CC]" *> pure Mod_CC
  <|> string "[AA]" *> pure Mod_AA
  <|> string "[TC]" *> pure Mod_TC
  <|> string "[TT]" *> pure Mod_TT
  <|> string "[CG]" *> pure Mod_CG
  <|> string "[GG]" *> pure Mod_GG
  <|> string "[AG]" *> pure Mod_AG
  <|> string "[GA]" *> pure Mod_GA
  <|> string "[CA]" *> pure Mod_CA
  <|> string "[CT]" *> pure Mod_CT
  <|> string "[TA]" *> pure Mod_TA
  <|> string "[AAA]" *> pure Mod_AAA
  <|> string "[AAC]" *> pure Mod_AAC
  <|> string "[ACT]" *> pure Mod_ACT
  <|> string "[ATC]" *> pure Mod_ATC
  <|> string "[ATG]" *> pure Mod_ATG
  <|> string "[CAG]" *> pure Mod_CAG
  <|> string "[AGA]" *> pure Mod_AGA
  <|> string "[CAT]" *> pure Mod_CAT
  <|> string "[CCG]" *> pure Mod_CCG
  <|> string "[CGT]" *> pure Mod_CGT
  <|> string "[CTG]" *> pure Mod_CTG
  <|> string "[GAA]" *> pure Mod_GAA
  <|> string "[GAC]" *> pure Mod_GAC
  <|> string "[GCT]" *> pure Mod_GCT
  <|> string "[GGT]" *> pure Mod_GGT
  <|> string "[GTT]" *> pure Mod_GTT
  <|> string "[TAC]" *> pure Mod_TAC
  <|> string "[TCT]" *> pure Mod_TCT
  <|> string "[TGC]" *> pure Mod_TGC
  <|> string "[TGG]" *> pure Mod_TGG
  <|> string "[TTC]" *> pure Mod_TTC
  <|> string "[TTT]" *> pure Mod_TTT
  <|> unknownP

unknownP :: Parser Modification
unknownP = try $ do
    _ <- char '[' 
    m <- many1' $ satisfy (\c -> isAlphaNum c || c `elem` ['+', '-', '*', '_'])
    _ <- char ']'
    pure $ Unknown ("[" <> m <> "]")
