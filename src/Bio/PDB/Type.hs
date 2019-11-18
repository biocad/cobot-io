module Bio.PDB.Type where

import           Control.DeepSeq (NFData (..))
import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Data.Vector     (Vector)
import           GHC.Generics    (Generic)

-- * Read PDB specification [here](http://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html).

data PDB = PDB { title       :: Text
               , models      :: Vector Model
               , remarks     :: Map RemarkCode RemarkData
               , otherFields :: Map FieldType FieldData
               }
  deriving (Show, Eq, Generic, NFData)

type RemarkCode = Int
type RemarkData = Vector Text

type FieldData = Vector Text
data FieldType
   =
   -- Title Section (except TITLE and REMARKS)
     HEADER
   | OBSLTE
   | SPLIT
   | CAVEAT
   | COMPND
   | SOURCE
   | KEYWDS
   | EXPDTA
   | NUMMDL
   | MDLTYP
   | AUTHOR
   | REVDAT
   | SPRSDE
   | JRNL
   -- Primary Structure Section
   | DBREF
   | DBREF1
   | DBREF2
   | SEQADV
   | SEQRES
   | MODRES
   -- Heterogen Section
   | HET
   | FORMUL
   | HETNAM
   | HETSYN
   -- Secondary Structure Section
   | HELIX
   | SHEET
   -- Connectivity Annotation Section
   | SSBOND
   | LINK
   | CISPEP
   -- Miscellaneous Features Section
   | SITE
   -- Crystallographic and Coordinate Transformation Section
   | CRYST1
   | MTRIXn
   | ORIGXn
   | SCALEn
   -- Bookkeeping Section
   | MASTER
  deriving (Show, Eq, Read, Generic, NFData, Ord)

type Model = Vector Chain

type Chain = Vector Atom

data Atom = Atom { atomSerial     :: Int     -- ^ Atom serial number.
                 , atomName       :: Text    -- ^ Atom name.
                 , atomAltLoc     :: Char    -- ^ Alternate location indicator.
                 , atomResName    :: Text    -- ^ Residue name.
                 , atomChainID    :: Char    -- ^ Chain identifier.
                 , atomResSeq     :: Int     -- ^ Residue sequence number.
                 , atomICode      :: Char    -- ^ Code for insertion of residues.
                 , atomX          :: Float   -- ^ Orthogonal coordinates for X in Angstroms.
                 , atomY          :: Float   -- ^ Orthogonal coordinates for Y in Angstroms.
                 , atomZ          :: Float   -- ^ Orthogonal coordinates for Z in Angstroms.
                 , atomOccupancy  :: Float   -- ^ Occupancy.
                 , atomTempFactor :: Float   -- ^ Temperature factor.
                 , atomElement    :: Text    -- ^ Element symbol, right-justified.
                 , atomCharge     :: Text    -- ^ Charge on the atom.
                 }
  deriving (Show, Eq, Generic, NFData)
