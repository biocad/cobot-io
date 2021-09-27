module Bio.GB.Type
  ( GenBankSequence (..)
  , Meta (..)
  , Form (..)
  , Locus (..)
  , Version (..)
  , Source (..)
  , Reference (..)
  , Feature (..)
  , Parser
  ) where

import Bio.Sequence    (IsMarking, MarkedSequence)
import Control.DeepSeq (NFData)
import Data.Text       (Text)
import Data.Void       (Void)
import GHC.Generics    (Generic)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

-- | Type that represents contents of .gb file that is used to store information about
-- genetic constructions.
--
data GenBankSequence
  = GenBankSequence
      { meta  :: Meta
        -- ^ meta-information about the sequence
      , gbSeq :: MarkedSequence Feature Char
        -- ^ sequence that is marked by 'Feature's
      }
  deriving (Eq, Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Block with meta-information.
--------------------------------------------------------------------------------

-- | Meta-information about sequence.
--
data Meta
  = Meta
      { locus      :: Locus
        -- ^ general info about sequence
      , definition :: Maybe Text
        -- ^ brief description of sequence
      , accession  :: Maybe Text
        -- ^ the unique identifier for a sequence record
      , version    :: Maybe Version
        -- ^ id of sequence in GenBank database
      , keywords   :: Maybe Text
        -- ^ word or phrase describing the sequence
      , source     :: Maybe Source
        -- ^ free-format information including an abbreviated form of the organism name,
        --   sometimes followed by a molecule type
      , references :: [Reference]
        -- ^ publications by the authors of the sequence that discuss the data reported in the record
      , comments   :: [Text]
        -- ^ comments about the sequence (note that there can be (!!!) empty comments)
      }
  deriving (Eq, Show, Generic, NFData)

-- | First line that should be present in every .gb file. Contains general info about sequence.
--
data Locus
  = Locus
      { name             :: Text
        -- ^ name of sequence
      , len              :: Int
        -- ^ length of sequence
      , molType          :: Text
        -- ^ type of molecule that is sequenced
      , form             :: Maybe Form
        -- ^ form of sequence
      , gbDivision       :: Maybe Text
        -- ^ GenBank division to which a record belongs
      , modificationDate :: Text
        -- ^ date of last modification of sequence
      }
  deriving (Eq, Show, Generic, NFData)

-- | At this moment there are two known (to me)
-- forms of seuqences that can be present in .gb file.
--
data Form
  = Linear
  | Circular
  deriving (Eq, Show, Generic, NFData)

-- | Id of sequence in GenBank database.
--
data Version
  = Version
      { versionT :: Text
        -- ^ id itself
      , gbId     :: Maybe Text
        -- ^ GenInfo Identifier that is assigned when sequence changes
      }
  deriving (Eq, Show, Generic, NFData)

-- | Information about source of this sequence.
--
data Source
  = Source
      { sourceT  :: Text
        -- ^ free-format (as if all this format is not too much "free format") information
        -- including an abbreviated form of the organism name,
        -- sometimes followed by a molecule type
      , organism :: Maybe Text
        -- ^ the formal scientific name for the source organism
      }
  deriving (Eq, Show, Generic, NFData)

-- | Publications by the authors of the sequence that discuss the data reported in the record.
--
data Reference
  = Reference
      { referenceT :: Text
        -- ^ reference itself
      , authors    :: Maybe Text
        -- ^ list of authors in the order in which they appear in the cited article
      , title      :: Maybe Text
        -- ^ title of the published work
      , journal    :: Maybe Text
        -- ^ MEDLINE abbreviation of the journal name
      , pubmed     :: Maybe Text
        -- ^ PubMed Identifier
      }
  deriving (Eq, Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Block with FEATURES table.
--
-- FEATURES table contains information about genes and gene products, as well as regions of biological
-- significance reported in the sequence. These can include regions of the sequence
-- that code for proteins and RNA molecules, as well as a number of other features.
-- More about FEATURES table: http://www.insdc.org/documents/feature_table.html
--------------------------------------------------------------------------------

-- | One single feature.
--
data Feature
  = Feature
      { fName  :: Text
        -- ^ main information about feature
      , fProps :: [(Text, Text)]
        -- ^ properties of feature (such as "label", "gene", "note" etc.)
      }
  deriving (Eq, Show, Ord, Generic, NFData)

instance IsMarking Feature
