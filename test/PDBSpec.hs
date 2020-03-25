{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PDBSpec where

import           Bio.PDB       (modelsFromPDBFile)
import qualified Bio.PDB.Type as PDB (Atom (..), FieldType (..), PDB (..), Model (..))
import           Bio.MAE       (modelsFromMaeFile)
import           Bio.PDB.Reader (fromFilePDB)
import           Bio.Structure (Model (..), Chain(..), Residue(..), Bond(..), GlobalID(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either (fromRight)
import           Data.Vector (Vector)
import qualified Data.Vector as V (head, length, toList, concatMap, fromList)
import           Data.List (groupBy, sortOn, find)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map.Strict
import           Test.Hspec


rawPDBToModelConversionSpec = describe "Cobot Model from raw PDB" $ do
  modelFromPDB <- runIO $ firstPDBModel "test/PDB/1PPE_I.pdb"
  modelFromMae <- runIO $ firstMaeModel "test/PDB/1PPE_I.mae"
  let (pdbBondCount, pdbChainCount, pdbAtomCount) = getStats modelFromPDB
  let (maeBondCount, _, _) = getStats modelFromMae

  it "should have correct number of atoms" $ pdbAtomCount `shouldBe` 436
  it "should have correct number of chains" $ pdbChainCount `shouldBe` 1
  it "should restore bonds correctly" $ pdbBondCount `shouldBe` maeBondCount
  where
    getStats :: Model -> (Int, Int, Int)
    getStats model = (bondCount, chainCount, atomCount)
      where 
        bondCount = V.length $ modelBonds model
        chainCount = V.length $ modelChains model
        atomCount = V.length . V.concatMap (V.concatMap resAtoms . chainResidues) $ modelChains model

tripeptides :: [String]
tripeptides = ["ALA_3", "ARG_3", "ASN_3", "ASP_3", "CYS_3", "GLN_3", "GLU_3", "GLY_3", "HID_3", "HIE_3", "HIP_3", "ILE_3", "LEU_3", "LYS_3", "MET_3", "PHE_3", "PRO_3", "SER_3", "THR_3", "TRP_3", "TYR_3", "VAL_3"]

bondsRestoringSpec = describe "Bonds should be restored correctly in tripeptides" $ do
  sequence_ $ checkTripeptide <$> tripeptides

checkTripeptide :: String -> SpecWith (Arg Expectation)
checkTripeptide tripeptideName = do
  modelFromMae <- runIO . firstMaeModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".mae"
  modelFromPDB <- runIO . firstPDBModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".pdb"

  let pdbBondCount = V.length $ modelBonds modelFromPDB
  let maeBondCount = V.length $ modelBonds modelFromMae

  it tripeptideName $ pdbBondCount `shouldBe` maeBondCount

firstPDBModel :: (MonadIO m) => FilePath -> m Model
firstPDBModel filepath = do
  eitherPDB <- modelsFromPDBFile filepath
  let (_, models) = fromRight undefined eitherPDB

  pure $ V.head models

firstMaeModel :: (MonadIO m) => FilePath -> m Model
firstMaeModel filepath = do
  eitherMae <- modelsFromMaeFile filepath
  pure . V.head $ fromRight undefined eitherMae