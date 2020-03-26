{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bio.Structure
  ( SecondaryStructure (..)
  , Atom (..), Bond (..)
  , Residue (..), Chain (..), Model (..)
  , StructureModels (..), StructureSerializable (..)
  , LocalID (..)
  , GlobalID (..)
  , atoms, localBonds
  , residues
  , chains, globalBonds
  ) where

import           Control.DeepSeq (NFData (..))
import           Control.Lens    (makeLensesFor)
import           Data.Text       (Text)
import           Data.Vector     (Vector)
import           GHC.Generics    (Generic)
import           Linear.V3       (V3)

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

newtype GlobalID = GlobalID { getGlobalID :: Int }
  deriving (Eq, Show, Ord, Generic, NFData)

newtype LocalID  = LocalID { getLocalID :: Int }
  deriving (Eq, Show, Ord, Generic, NFData)

-- | Generic atom representation
--
data Atom = Atom { atomId       :: GlobalID -- ^ global identifier
                 , atomName     :: Text     -- ^ IUPAC atom name
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
data Bond m = Bond { bondStart :: m    -- ^ index of first incident atom
                   , bondEnd   :: m    -- ^ index of second incident atom
                   , bondOrder :: Int  -- ^ the order of chemical bond
                   }
  deriving (Show, Eq, Functor, Generic)

instance Ord (Bond LocalID) where
    (Bond (LocalID x) (LocalID y) _) <= (Bond (LocalID x') (LocalID y') _) | x == x'   = y <= y'
                                                                           | otherwise = x <= x'

instance Ord (Bond GlobalID) where
    (Bond (GlobalID x) (GlobalID y) _) <= (Bond (GlobalID x') (GlobalID y') _) | x == x'   = y <= y'
                                                                               | otherwise = x <= x'

instance NFData a => NFData (Bond a)

-- | A set of atoms, organized to a residues
--
data Residue = Residue { resName         :: Text                  -- ^ residue name
                       , resAtoms        :: Vector Atom           -- ^ a set of residue atoms
                       , resBonds        :: Vector (Bond LocalID) -- ^ a set of residue bonds with local identifiers (position in 'resAtoms')
                       , resSecondary    :: SecondaryStructure    -- ^ residue secondary structure
                       , resChemCompType :: Text                  -- ^ chemical component type
                       }
  deriving (Show, Eq, Generic, NFData)

$(makeLensesFor [("resAtoms", "atoms"), ("resBonds", "localBonds")] ''Residue)

-- | Chain organizes linear structure of residues
--
data Chain = Chain { chainName     :: Text              -- ^ name of a chain
                   , chainResidues :: Vector Residue    -- ^ residues of a chain
                   }
  deriving (Show, Eq, Generic, NFData)

$(makeLensesFor [("chainResidues", "residues")] ''Chain)

-- | Model represents a single experiment of structure determination
--
data Model = Model { modelChains :: Vector Chain           -- ^ chains in the model
                   , modelBonds  :: Vector (Bond GlobalID) -- ^ bonds with global identifiers (field `atomId` in 'Atom')
                   }
  deriving (Show, Eq, Generic, NFData)

$(makeLensesFor [("modelChains", "chains"), ("modelBonds", "globalBonds")] ''Model)

-- | Convert any format-specific data to an intermediate representation of structure
class StructureModels a where
    -- | Get an array of models
    modelsOf :: a -> Vector Model

-- | Serialize an intermediate representation of sequence to some specific format
class StructureSerializable a where
    -- | Serialize an array of models to some format
    serializeModels :: Vector Model -> a
