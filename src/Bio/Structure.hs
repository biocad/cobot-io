{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Bio.Structure
  ( SecondaryStructure (..)
  , Atom (..), Bond (..)
  , Residue (..), Chain (..), Model (..)
  , StructureModels (..), StructureSerializable (..)
  ) where

import           Data.Array      ( Array )
import           Data.Text       ( Text )
import           GHC.Generics    ( Generic )
import           Control.DeepSeq ( NFData (..) )
import           Linear.V3       ( V3 )

-- | Protein secondary structure
--
data SecondaryStructure = PiHelix       -- ^ pi helix
                        | Bend          -- ^ bend
                        | AlphaHelix    -- ^ alpha helix
                        | Extended      -- ^ extended
                        | ThreeTenHelix -- ^ 3-10 helix
                        | Bridge        -- ^ brigde
                        | Turn          -- ^ turn
                        | Coil          -- ^ coil
                        | Undefined     -- ^ unknown structure
  deriving (Show, Eq, Generic)

instance NFData SecondaryStructure

-- | Generic atom representation
--
data Atom = Atom { atomName     :: Text     -- ^ IUPAC atom name 
                 , atomElement  :: Text     -- ^ atom chemical element
                 , atomCoords   :: V3 Float -- ^ 3D coordinates of atom
                 , formalCharge :: Int      -- ^ Formal charge of atom
                 , bFactor      :: Float    -- ^ B-factor of atom
                 , occupancy    :: Float    -- ^ the amount of each conformation that is observed in the crystal
                 }
  deriving (Show, Eq, Generic)

instance NFData Atom

-- | Generic chemical bond
--
data Bond = Bond { bondStart :: Int  -- ^ index of first incident atom
                 , bondEnd   :: Int  -- ^ index of second incident atom
                 , bondOrder :: Int  -- ^ the order of chemical bond
                 }
  deriving (Show, Eq, Generic)

instance NFData Bond

-- | A set of atoms, organized to a residues
--
data Residue = Residue { resName      :: Text               -- ^ residue name
                       , resAtoms     :: Array Int Atom     -- ^ a set of residue atoms
                       , resBonds     :: Array Int Bond     -- ^ a set of residue bonds
                       , resSecondary :: SecondaryStructure -- ^ residue secondary structure
                       }
  deriving (Show, Eq, Generic, NFData)

-- | Chain organizes linear structure of residues
--
data Chain = Chain { chainName     :: Text              -- ^ name of a chain
                   , chainResidues :: Array Int Residue -- ^ residues of a chain
                   }
  deriving (Show, Eq, Generic, NFData)

-- | Model represents a single experiment of structure determination
--
newtype Model = Model { modelChains :: Array Int Chain }
  deriving (Show, Eq, Generic, NFData)

-- | Convert any format-specific data to an intermediate representation of structure
class StructureModels a where
    -- | Get an array of models
    modelsOf :: a -> Array Int Model

-- | Serialize an intermediate representation of sequence to some specific format
class StructureSerializable a where
    -- | Serialize an array of models to some format
    serializeModels :: Array Int Model -> a