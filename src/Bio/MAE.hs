{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.MAE
  ( Block (..), FromMaeValue (..)
  , Mae (..), MaeValue (..)
  , Table (..)
  , fromFile
  , fromText
  , maeP
  ) where

import           Bio.MAE.Parser
import           Bio.MAE.Type           (Block (..), FromMaeValue (..),
                                         Mae (..), MaeValue (..), Table (..))
import           Bio.Structure          (Atom (..), Bond (..), Chain (..),
                                         GlobalID (..), LocalID (..),
                                         Model (..), Residue (..),
                                         SecondaryStructure (..),
                                         StructureModels (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (bimap, first)
import           Data.Function          (on)
import qualified Data.List              as L (find, groupBy, sortOn)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M (fromList, lookup, member, (!))
import           Data.Maybe             (catMaybes, fromJust)
import           Data.Text              (Text)
import qualified Data.Text              as T (head, init, last, null, pack,
                                              strip, tail)
import qualified Data.Text.IO           as TIO (readFile)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V (fromList)
import           Linear.V3              (V3 (..))

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
      unsafeGetFromContentsMap :: FromMaeValue a => Map Text [MaeValue] -> Text -> Int -> a
      unsafeGetFromContentsMap m name i = unsafeFromMaeValue $ (m M.! name) !! i

      getFromContentsMap :: FromMaeValue a => Map Text [MaeValue] -> Text -> Int -> Maybe a
      getFromContentsMap m name i = fromMaeValue $ (m M.! name) !! i

      blockToModel :: Block -> Model
      blockToModel Block{..} = Model (atomsTableToChains atomsTable) bonds
        where
          atomsTable    = findTable "m_atom"
          numberOfAtoms = length $ atomsTable M.! "r_m_x_coord"

          bondsTable         = findTable "m_bond"
          (bondGraph, bonds) = bondsTableToGlobalBonds bondsTable

          findTable :: Text -> Map Text [MaeValue]
          findTable name = contents $ fromJust $ L.find ((== name) . tableName) tables

          stripQuotes :: Text -> Text
          stripQuotes t | not (T.null t) && T.head t == T.last t, T.last t == '\"' = T.strip $ T.init $ T.tail t
                        | otherwise                                                = T.strip t

          toGroupsOn :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
          toGroupsOn f = L.groupBy ((==) `on` f) . L.sortOn f

          bondsTableToGlobalBonds :: Map Text [MaeValue] -> (Map Int [(Int, Int)], Vector (Bond GlobalID))
          bondsTableToGlobalBonds m = bimap toMap V.fromList bonds'
            where
              numberOfBonds = length $ m M.! "i_m_from"
              bonds'        = unzip $ fmap indexToBond [0 .. numberOfBonds - 1]

              toMap :: [(Int, (Int, Int))] -> Map Int [(Int, Int)]
              toMap = M.fromList . fmap (\l@((k, _) : _) -> (k, fmap snd l)) . toGroupsOn fst

              indexToBond :: Int -> ((Int, (Int, Int)), Bond GlobalID)
              indexToBond i = ((x, (y, o)), Bond (GlobalID x) (GlobalID y) o)
                where
                  x = getFromContentsI "i_m_from" - 1
                  y = getFromContentsI "i_m_to" - 1
                  o = getFromContentsI "i_m_order"

                  getFromContentsI :: FromMaeValue a => Text -> a
                  getFromContentsI = flip (unsafeGetFromContentsMap m) i

          atomsTableToChains :: Map Text [MaeValue] -> Vector Chain
          atomsTableToChains m = V.fromList $ fmap groupToChain groupedByChains
            where
              groupedByChains = toGroupsOn (unsafeGetFromContents @Text "s_m_chain_name") [0 .. numberOfAtoms - 1]

              unsafeGetFromContents :: FromMaeValue a => Text -> Int -> a
              unsafeGetFromContents = unsafeGetFromContentsMap m

              getFromContents :: FromMaeValue a => Text -> Int -> Maybe a
              getFromContents = getFromContentsMap m

              groupToChain :: [Int] -> Chain
              groupToChain []            = error "Group that is result of List.groupBy can't be empty."
              groupToChain group@(h : _) = Chain name residues
                where
                  name = stripQuotes $ unsafeGetFromContents "s_m_chain_name" h

                  groupedByResidues = toGroupsOn by group
                  residues          = V.fromList $ fmap groupToResidue groupedByResidues

                  by :: Int -> (Int, Text)
                  by i = (unsafeGetFromContents "i_m_residue_number" i, unsafeGetFromContents "s_m_pdb_residue_name" i)

              groupToResidue :: [Int] -> Residue
              groupToResidue []            = error "Group that is result of List.groupBy can't be empty."
              groupToResidue group@(h : _) = Residue name atoms (V.fromList localBonds) secondary chemCompType
                where
                  name  = stripQuotes $ unsafeGetFromContents "s_m_pdb_residue_name" h
                  atoms = V.fromList $ fmap indexToAtom group

                  localInds     = [0 .. length group - 1]
                  globalToLocal = M.fromList $ zip group localInds
                  bondsParts    = fmap (`M.lookup` bondGraph) group
                  localBonds    = concat $ catMaybes $ zipWith (\l x -> fmap (concatMap (toLocalBond x)) l) bondsParts localInds

                  toLocalBond :: Int -> (Int, Int) -> [Bond LocalID]
                  toLocalBond x (y, o) | y `elem` group = pure $ Bond (LocalID x)
                                                                      (LocalID $ globalToLocal M.! y)
                                                                      o
                                       | otherwise          = []

                  secondary    = Undefined
                  chemCompType = mempty

              indexToAtom :: Int -> Atom
              indexToAtom i = Atom (GlobalID i)
                                   (stripQuotes $ getFromContentsI "s_m_pdb_atom_name")
                                   (elIndToElement M.! getFromContentsI "i_m_atomic_number")
                                   coords
                                   (getFromContentsIWithDef 0 "i_m_formal_charge")
                                   (getFromContentsIWithDef 0 "r_m_pdb_tfactor")
                                   (getFromContentsIWithDef 0 "r_m_pdb_occupancy")
                where
                  getFromContentsI :: FromMaeValue a => Text -> a
                  getFromContentsI = flip unsafeGetFromContents i

                  getFromContentsIWithDef :: FromMaeValue a => a -> Text -> a
                  getFromContentsIWithDef def n | n `M.member` m = maybe def id $ getFromContents n i
                                                | otherwise      = def

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
