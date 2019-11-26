{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.MAE
  ( module T
  , fromFile
  , fromText
  , maeP
  ) where

import           Bio.MAE.Parser
import           Bio.MAE.Type            as T (Block (..), FromMaeValue (..),
                                               Mae (..), MaeValue (..),
                                               Table (..))
import           Bio.Structure           (Atom (..), Bond (..), Chain (..),
                                          GlobalID (..), LocalID (..),
                                          Model (..), Residue (..),
                                          SecondaryStructure (..),
                                          StructureModels (..))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Attoparsec.Text    (parseOnly)
import           Data.Bifunctor          (bimap, first)
import           Data.Function           (on)
import qualified Data.List               as L (find, groupBy)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M (fromList, (!))
import           Data.Maybe              (fromJust)
import           Data.Text               (Text)
import qualified Data.Text               as T (pack, strip)
import qualified Data.Text.IO            as TIO (readFile)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V (fromList)
import           Linear.V3               (V3 (..))

import           Math.Grads.GenericGraph (GenericGraph)
import qualified Math.Grads.Graph        as G (fromList, (!.))

-- | Reads 'Mae' from givem file.
--
fromFile :: MonadIO m => FilePath -> m Mae
fromFile f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly maeP

-- | Reads 'Mae' from 'Text'.
--
fromText :: Text -> Either Text Mae
fromText = first T.pack . parseOnly maeP

instance StructureModels Mae where
  modelsOf Mae{..} = V.fromList $ fmap blockToModel blocks
    where
      getFromContentsMap :: FromMaeValue a => Map Text [MaeValue] -> Text -> Int -> a
      getFromContentsMap m name i = unsafeFromMaeValue $ (m M.! name) !! i

      blockToModel :: Block -> Model
      blockToModel Block{..} = Model (atomsTableToChains atomsTable) bonds
        where
          atomsTable    = findTable "m_atom"
          numberOfAtoms = length $ atomsTable M.! "r_m_x_coord"

          bondsTable         = findTable "m_bond"
          (bondGraph, bonds) = bondsTableToGlobalBonds bondsTable

          findTable :: Text -> Map Text [MaeValue]
          findTable name = contents $ fromJust $ L.find ((== name) . tableName) tables

          bondsTableToGlobalBonds :: Map Text [MaeValue] -> (GenericGraph Int Int, Vector (Bond GlobalID))
          bondsTableToGlobalBonds m = bimap (G.fromList . (,) ([0 .. numberOfAtoms])) V.fromList bonds'
            where
              numberOfBonds = length $ m M.! "i_m_from"
              bonds'        = unzip $ fmap indexToBond [0 .. numberOfBonds]

              indexToBond :: Int -> ((Int, Int, Int), Bond GlobalID)
              indexToBond i = ((x, y, o), Bond (GlobalID y) (GlobalID y) o)
                where
                  x = getFromContentsI "i_m_from" - 1
                  y = getFromContentsI "i_m_to" - 1
                  o = getFromContentsI "i_m_order"

                  getFromContentsI :: FromMaeValue a => Text -> a
                  getFromContentsI = flip (getFromContentsMap m) i

          atomsTableToChains :: Map Text [MaeValue] -> Vector Chain
          atomsTableToChains m = V.fromList $ fmap groupToChain groupedByChains
            where
              groupedByChains = L.groupBy (comparingOn @Text "s_m_chain_name") [0 .. numberOfAtoms]

              getFromContents :: FromMaeValue a => Text -> Int -> a
              getFromContents = getFromContentsMap m

              comparingOn :: forall a. (Eq a, FromMaeValue a) => Text -> Int -> Int -> Bool
              comparingOn name = (==) `on` (getFromContents name :: Int -> a)

              groupToChain :: [Int] -> Chain
              groupToChain []            = error "Group that is result of List.groupBy can't be empty."
              groupToChain group@(h : _) = Chain name residues
                where
                  name = getFromContents "s_m_chain_name" h

                  groupedByResidues = L.groupBy (comparingOn @Int "i_m_residue_number") [0 .. length group]
                  residues          = V.fromList $ fmap groupToResidue groupedByResidues

              groupToResidue :: [Int] -> Residue
              groupToResidue []            = error "Group that is result of List.groupBy can't be empty."
              groupToResidue group@(h : _) = Residue name atoms (V.fromList localBonds) secondary chemCompType
                where
                  name  = T.strip $ getFromContents "s_m_pdb_residue_name" h
                  atoms = V.fromList $ fmap indexToAtom group

                  localInds     = [0 .. length group]
                  globalToLocal = M.fromList $ zip group localInds
                  localBonds    = concat $ zipWith (\l x -> fmap (toLocalBond x) l) (fmap (bondGraph G.!.) group) localInds

                  toLocalBond :: Int -> (Int, Int) -> Bond LocalID
                  toLocalBond x (y, o) = Bond (LocalID $ globalToLocal M.! x)
                                              (LocalID $ globalToLocal M.! y)
                                              o

                  secondary    = Undefined
                  chemCompType = mempty

              indexToAtom :: Int -> Atom
              indexToAtom i = Atom (GlobalID i)
                                  (T.strip $ getFromContentsI "s_m_pdb_atom_name")
                                  (elIndToElement M.! getFromContentsI "i_m_atomic_number")
                                  coords
                                  (getFromContentsI "i_m_formal_charge")
                                  (getFromContentsI "r_m_pdb_tfactor")
                                  (getFromContentsI "r_m_pdb_occupancy")
                where
                  getFromContentsI :: FromMaeValue a => Text -> a
                  getFromContentsI = flip getFromContents i

                  coords :: V3 Float
                  coords = V3 (getFromContentsI "r_m_x_coord")
                              (getFromContentsI "r_m_y_coord")
                              (getFromContentsI "r_m_z_coord")

elIndToElement :: Map Int Text
elIndToElement = M.fromList $ zip [1 .. 118] [ "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne"
                                             , "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca"
                                             , "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn"
                                             , "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr"
                                             , "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn"
                                             , "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd"
                                             , "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb"
                                             , "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg"
                                             , "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th"
                                             , "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm"
                                             , "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds"
                                             , "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"
                                             ]
