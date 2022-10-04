module Bio.FASTA.Type
  ( Fasta
  , FastaItem (..)
  , ParsableFastaToken (..)
  , Modification (..)
  , ModItem (..)
  , modificationToString
  ) where

import Bio.Sequence    (BareSequence)
import Data.Text       (Text)
import Data.Void
import GHC.Generics    (Generic)
import Text.Megaparsec

-- | Type alias for FASTA file.
--  satisfies the following format : >(\s|\t)*[^\n\r]+(\s|\t)*(\n|\r)*((\w|\s)(\n|\r)*)*
--
type Fasta a = [FastaItem a]

-- | One record in FASTA file.
--
data FastaItem a
  = FastaItem
      { name :: Text
        -- ^ name of the sequence
      , sequ :: BareSequence a
        -- ^ bare sequence
      }
  deriving (Eq, Show, Functor)

class ParsableFastaToken a where
    parseToken :: (Char -> Bool) -> Parsec Void Text a

data ModItem
  = Mod Modification
  | Letter Char
  deriving (Eq, Show, Generic)

data Modification
  = Mod_A_Star
  | Mod_C_Star
  | Mod_G_Star
  | Mod_T_Star
  | Mod_rA
  | Mod_rC
  | Mod_rG
  | Mod_rU
  | Mod_Plus_A
  | Mod_Plus_C
  | Mod_Plus_G
  | Mod_Plus_T
  | Mod_rAf
  | Mod_rCf
  | Mod_rGf
  | Mod_rUf
  | Mod_mA
  | Mod_mC
  | Mod_mG
  | Mod_mU
  | Mod_mA_Star
  | Mod_mC_Star
  | Mod_mG_Star
  | Mod_mU_Star
  | Mod_dU
  | Mod_5Bio
  | Mod_iBio
  | Mod_56FAM
  | Mod_36FAM
  | Mod_5HEX
  | Mod_5TMR
  | Mod_3BHQ1
  | Mod_3BHQ2
  | Mod_5NH2
  | Mod_3NH2
  | Mod_5PO4
  | Mod_3PO4
  | Mod_3BioTEG
  | Mod_C12
  | Mod_NHSdT
  | Mod_5Mal
  | Mod_5thio
  | Mod_3thio
  | Mod_3azide
  | Mod_3alkine
  | Mod_5CholTEG
  | Mod_3CholTEG
  | Mod_5C10
  | Mod_5Alk
  | Mod_GC
  | Mod_GT
  | Mod_AT
  | Mod_TG
  | Mod_AC
  | Mod_CC
  | Mod_AA
  | Mod_TC
  | Mod_TT
  | Mod_CG
  | Mod_GG
  | Mod_AG
  | Mod_GA
  | Mod_CA
  | Mod_CT
  | Mod_TA
  | Mod_AAA
  | Mod_AAC
  | Mod_ACT
  | Mod_ATC
  | Mod_ATG
  | Mod_CAG
  | Mod_AGA
  | Mod_CAT
  | Mod_CCG
  | Mod_CGT
  | Mod_CTG
  | Mod_GAA
  | Mod_GAC
  | Mod_GCT
  | Mod_GGT
  | Mod_GTT
  | Mod_TAC
  | Mod_TCT
  | Mod_TGC
  | Mod_TGG
  | Mod_TTC
  | Mod_TTT
  | Unknown String
  deriving (Eq, Show, Ord, Generic)

modificationToString :: Modification -> String
modificationToString Mod_A_Star   = "[A*]"
modificationToString Mod_C_Star   = "[C*]"
modificationToString Mod_G_Star   = "[G*]"
modificationToString Mod_T_Star   = "[T*]"
modificationToString Mod_rA       = "[rA]"
modificationToString Mod_rC       = "[rC]"
modificationToString Mod_rG       = "[rG]"
modificationToString Mod_rU       = "[rU]"
modificationToString Mod_Plus_A   = "[+A]"
modificationToString Mod_Plus_C   = "[+C]"
modificationToString Mod_Plus_G   = "[+G]"
modificationToString Mod_Plus_T   = "[+T]"
modificationToString Mod_rAf      = "[rAf]"
modificationToString Mod_rCf      = "[rCf]"
modificationToString Mod_rGf      = "[rGf]"
modificationToString Mod_rUf      = "[rUf]"
modificationToString Mod_mA       = "[mA]"
modificationToString Mod_mC       = "[mC]"
modificationToString Mod_mG       = "[mG]"
modificationToString Mod_mU       = "[mU]"
modificationToString Mod_mA_Star  = "[mA*]"
modificationToString Mod_mC_Star  = "[mC*]"
modificationToString Mod_mG_Star  = "[mG*]"
modificationToString Mod_mU_Star  = "[mU*]"
modificationToString Mod_dU       = "[dU]"
modificationToString Mod_5Bio     = "[5Bio]"
modificationToString Mod_iBio     = "[iBio]"
modificationToString Mod_56FAM    = "[56FAM]"
modificationToString Mod_36FAM    = "[36FAM]"
modificationToString Mod_5HEX     = "[5HEX]"
modificationToString Mod_5TMR     = "[5TMR]"
modificationToString Mod_3BHQ1    = "[3BHQ1]"
modificationToString Mod_3BHQ2    = "[3BHQ2]"
modificationToString Mod_5NH2     = "[5NH2]"
modificationToString Mod_3NH2     = "[3NH2]"
modificationToString Mod_5PO4     = "[5PO4]"
modificationToString Mod_3PO4     = "[3PO4]"
modificationToString Mod_3BioTEG  = "[3BioTEG]"
modificationToString Mod_C12      = "[C12]"
modificationToString Mod_NHSdT    = "[NHSdT]"
modificationToString Mod_5Mal     = "[5Mal]"
modificationToString Mod_5thio    = "[5thio]"
modificationToString Mod_3thio    = "[3thio]"
modificationToString Mod_3azide   = "[3azide]"
modificationToString Mod_3alkine  = "[3alkine]"
modificationToString Mod_5CholTEG = "[5CholTEG]"
modificationToString Mod_3CholTEG = "[3CholTEG]"
modificationToString Mod_5C10     = "[5C10]"
modificationToString Mod_5Alk     = "[5Alk]"
modificationToString Mod_GC       = "[GC]"
modificationToString Mod_GT       = "[GT]"
modificationToString Mod_AT       = "[AT]"
modificationToString Mod_TG       = "[TG]"
modificationToString Mod_AC       = "[AC]"
modificationToString Mod_CC       = "[CC]"
modificationToString Mod_AA       = "[AA]"
modificationToString Mod_TC       = "[TC]"
modificationToString Mod_TT       = "[TT]"
modificationToString Mod_CG       = "[CG]"
modificationToString Mod_GG       = "[GG]"
modificationToString Mod_AG       = "[AG]"
modificationToString Mod_GA       = "[GA]"
modificationToString Mod_CA       = "[CA]"
modificationToString Mod_CT       = "[CT]"
modificationToString Mod_TA       = "[TA]"
modificationToString Mod_AAA      = "[AAA]"
modificationToString Mod_AAC      = "[AAC]"
modificationToString Mod_ACT      = "[ACT]"
modificationToString Mod_ATC      = "[ATC]"
modificationToString Mod_ATG      = "[ATG]"
modificationToString Mod_CAG      = "[CAG]"
modificationToString Mod_AGA      = "[AGA]"
modificationToString Mod_CAT      = "[CAT]"
modificationToString Mod_CCG      = "[CCG]"
modificationToString Mod_CGT      = "[CGT]"
modificationToString Mod_CTG      = "[CTG]"
modificationToString Mod_GAA      = "[GAA]"
modificationToString Mod_GAC      = "[GAC]"
modificationToString Mod_GCT      = "[GCT]"
modificationToString Mod_GGT      = "[GGT]"
modificationToString Mod_GTT      = "[GTT]"
modificationToString Mod_TAC      = "[TAC]"
modificationToString Mod_TCT      = "[TCT]"
modificationToString Mod_TGC      = "[TGC]"
modificationToString Mod_TGG      = "[TGG]"
modificationToString Mod_TTC      = "[TTC]"
modificationToString Mod_TTT      = "[TTT]"
modificationToString (Unknown s)  = s
