{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PDBSpec where

import           Bio.PDB       (modelsFromPDBFile)
import qualified Bio.PDB.Type as PDB (Atom (..), FieldType (..), PDB (..), Model (..))
import           Bio.MAE       (modelsFromMaeFile)
import           Bio.PDB.Reader (fromFilePDB)
import           Bio.Structure (Model (..), Chain(..), Residue(..), Atom(..), Bond(..), GlobalID(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either (fromRight)
import           Data.Vector (Vector)
import qualified Data.Vector as V (head, length, toList, concatMap, fromList)
import           Data.List (groupBy, sortOn, find)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M (fromList)
import           Data.Set (Set)
import qualified Data.Set as S
import           Test.Hspec


rawPDBToModelConversionSingleChainSpec :: SpecWith ()
rawPDBToModelConversionSingleChainSpec = describe "Cobot Model from raw single chain PDB" $ do
  modelFromPDB <- runIO $ firstPDBModel "test/PDB/1PPE_I.pdb"
  modelFromMae <- runIO $ firstMaeModel "test/PDB/1PPE_I.mae"
  let (pdbBondCount, pdbChainCount, pdbAtomCount) = getStats modelFromPDB
  let (maeBondCount, _, _) = getStats modelFromMae

  it "Should have correct number of atoms" $ pdbAtomCount `shouldBe` 436
  it "Should have correct number of chains" $ pdbChainCount `shouldBe` 1
  it "Should restore bonds correctly" $ pdbBondCount `shouldBe` maeBondCount

-- TODO: validate local bonds in the same manner
bondsRestoringTripeptideSpec :: SpecWith ()
bondsRestoringTripeptideSpec = describe "Bonds should be restored correctly in tripeptides" $
  sequence_ $ checkTripeptide <$> tripeptides
    where
      tripeptides :: [String]
      tripeptides = ["ALA_3", "ARG_3", "ASN_3", "ASP_3", "CYS_3", "GLN_3", "GLU_3", "GLY_3", "HID_3", "HIE_3", "HIP_3", 
                    "ILE_3", "LEU_3", "LYS_3", "MET_3", "PHE_3", "PRO_3", "SER_3", "THR_3", "TRP_3", "TYR_3", "VAL_3"]
      checkTripeptide :: String -> SpecWith (Arg Expectation)
      checkTripeptide tripeptideName = do
        modelFromMae <- runIO . firstMaeModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".mae"
        modelFromPDB <- runIO . firstPDBModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".pdb"

        let pdbBondCount = V.length $ modelBonds modelFromPDB
        let maeBondCount = V.length $ modelBonds modelFromMae

        it (tripeptideName ++ " equal bond count in Mae and PDB") $ pdbBondCount `shouldBe` maeBondCount


bondsRestoringBiggerMoleculesSpec :: SpecWith ()
bondsRestoringBiggerMoleculesSpec = describe "Bonds should be restored correctly in bigger molecules" $ do
  checkBiggerMolecule "3mxw_ab_b"
  checkBiggerMolecule "1vfb_ab_b"
  checkBiggerMolecule "4dn4_ag_b"
  where
    checkBiggerMolecule moleculeName = do
      modelFromPDB <- runIO . firstPDBModel $ "test/PDB/BondsRestoring/" ++ moleculeName ++ ".pdb"
      modelFromMae <- runIO . firstMaeModel $ "test/PDB/BondsRestoring/" ++ moleculeName ++ ".mae"
      let (pdbBondCount, pdbChainCount, pdbAtomCount) = getStats modelFromPDB
      let (maeBondCount, _, _) = getStats modelFromMae

      it (moleculeName ++ " equal bond count in Mae and PDB") $ pdbBondCount `shouldBe` maeBondCount

      it (moleculeName ++ " no dublicate bonds") $ length (doubleBonds modelFromPDB) `shouldBe` 0

      let _bondSetPDB = bondSetPDB modelFromPDB
      let _bondSetMae = bondSetMae modelFromMae
      let diffMaePDB = S.difference _bondSetMae _bondSetPDB
      let diffPDBMae = S.difference _bondSetPDB _bondSetMae

      it (moleculeName ++ " difference in Mae and PDB bond sets") $ S.size diffMaePDB `shouldBe` 0
      it (moleculeName ++ " difference in PDB and Mae bond sets") $ S.size diffPDBMae `shouldBe` 0


    doubleBonds :: Model -> [Bond GlobalID]
    doubleBonds Model{..} = doubleBonds' (V.toList modelBonds) []
      where
        doubleBonds' :: [Bond GlobalID] -> [Bond GlobalID] -> [Bond GlobalID]
        doubleBonds' [] acc = acc
        doubleBonds' (b:bs) acc = doubleBonds' bs . maybe acc (:acc) $ find (bondsEqual b) bs
        bondsEqual :: Bond GlobalID -> Bond GlobalID -> Bool
        bondsEqual b1 b2 = bondStart b1 == bondStart b2 && bondEnd b1 == bondEnd b2

    bondSetMae :: Model -> Set (Text,Text)
    bondSetMae Model{..} = bondSet (atomMap modelChains) (+1) modelBonds
    bondSetPDB :: Model -> Set (Text,Text)
    bondSetPDB Model{..} = bondSet (atomMap modelChains) id modelBonds

    atomMap :: Vector Chain -> Map Int (Text, Atom)
    atomMap chains = M.fromList $ concatMap chainAtomPreMap chains
      where
        chainAtomPreMap :: Chain -> [(Int, (Text, Atom))]
        chainAtomPreMap Chain{..} = V.toList . fmap (\a -> (getGlobalID $ atomId a, (chainName, a))) $ V.concatMap resAtoms chainResidues

    bondSet :: Map Int (Text, Atom) -> (Int -> Int) -> Vector (Bond GlobalID) -> Set (Text,Text)
    bondSet atomMap atomIdMap bonds = S.fromList $ do
      Bond{..} <- V.toList bonds
      let atomFromId = atomId $ atomMap ! getGlobalID bondStart
      let atomToId = atomId $ atomMap ! getGlobalID bondEnd
      [(atomFromId, atomToId), (atomToId, atomFromId)]
      where
        atomId :: (Text, Atom) -> Text
        atomId (chainId, Atom{..}) = chainId <> "_" <> atomName <> "_" <> T.pack (show . atomIdMap $ getGlobalID atomId)


getStats :: Model -> (Int, Int, Int)
getStats model = (bondCount, chainCount, atomCount)
  where 
    bondCount = V.length $ modelBonds model
    chainCount = V.length $ modelChains model
    atomCount = V.length . V.concatMap (V.concatMap resAtoms . chainResidues) $ modelChains model

firstPDBModel :: (MonadIO m) => FilePath -> m Model
firstPDBModel filepath = do
  eitherPDB <- modelsFromPDBFile filepath
  let (_, models) = fromRight undefined eitherPDB

  pure $ V.head models

firstMaeModel :: (MonadIO m) => FilePath -> m Model
firstMaeModel filepath = do
  eitherMae <- modelsFromMaeFile filepath
  pure . V.head $ fromRight undefined eitherMae