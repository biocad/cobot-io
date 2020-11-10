{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PDBSpec where

import           Bio.PDB                (modelsFromPDBFile)
import           Bio.MAE                (modelsFromMaeFile)
import           Bio.Structure          ( Model(..), Chain(..), Residue(..)
                                        , Atom(..), Bond(..), GlobalID(..), LocalID(..))

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Exception      (evaluate)
import           Control.DeepSeq        (force, NFData)

import           Data.Either            (fromRight)
import           Data.Vector            (Vector)
import qualified Data.Vector as V       (head, length, toList, concatMap)
import           Data.List              (find)
import           Data.Text              (Text)
import qualified Data.Text as T         (pack)
import           Data.Map.Strict        (Map, (!))
import qualified Data.Map.Strict as M   (fromList)
import           Data.Set               (Set)
import qualified Data.Set as S          (fromList, size, difference)

import           Test.Hspec

rawPDBToModelConversionSingleChainSpec :: SpecWith ()
rawPDBToModelConversionSingleChainSpec = describe "Cobot Model from raw single chain PDB" $ do
    let pdbStats = getStats <$> firstPDBModel "test/PDB/1PPE_I.pdb"
    let maeStats = getStats <$> firstMaeModel "test/PDB/1PPE_I.mae"
    beforeAll ((,) <$> pdbStats <*> maeStats) $ do
        it "Should have correct number of atoms" $ \((_, _, _, pdbAtomCount), _) ->
          pdbAtomCount `shouldBe` 436
        it "Should have correct number of chains" $ \((_, _, pdbChainCount, _), _) ->
          pdbChainCount `shouldBe` 1
        it "Should restore bonds correctly" $ \((pdbBondCount, _, _, _), (maeBondCount, _, _, _)) ->
          pdbBondCount `shouldBe` maeBondCount

-- tripeptides are not checked as in checkBiggerMolecule
-- because there are inconsistencies in atom numbers between pdb and mae
bondsRestoringTripeptideSpec :: SpecWith ()
bondsRestoringTripeptideSpec = describe "Bonds should be restored correctly in tripeptides" $
  sequence_ $ checkTripeptide <$> tripeptides
    where
      tripeptides :: [String]
      tripeptides = ["ALA_3", "ARG_3", "ASN_3", "ASP_3", "CYS_3", "GLN_3", "GLU_3", "GLY_3", "HID_3", "HIE_3", "HIP_3",
                     "ILE_3", "LEU_3", "LYS_3", "MET_3", "PHE_3", "PRO_3", "SER_3", "THR_3", "TRP_3", "TYR_3", "VAL_3"]
      checkTripeptide :: String -> SpecWith (Arg Expectation)
      checkTripeptide tripeptideName = it (tripeptideName ++ " equal bond count in Mae and PDB") $ do
          modelFromMae <- firstMaeModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".mae"
          modelFromPDB <- firstPDBModel $ "test/PDB/BondsRestoring/" ++ tripeptideName ++ ".pdb"

          let pdbBondCount = V.length $ modelBonds modelFromPDB
          let maeBondCount = V.length $ modelBonds modelFromMae

          pdbBondCount `shouldBe` maeBondCount

bondsRestoringBiggerMoleculesSpec :: SpecWith ()
bondsRestoringBiggerMoleculesSpec = describe "Bonds should be restored correctly in bigger molecules" $ do
  checkBiggerMolecule "3mxw_ab_b"
  checkBiggerMolecule "1vfb_ab_b"
  checkBiggerMolecule "4dn4_ag_b"
  where
    checkBiggerMolecule moleculeName = do
      let pdbIO = firstPDBModel $ "test/PDB/BondsRestoring/" ++ moleculeName ++ ".pdb"
      let maeIO = firstMaeModel $ "test/PDB/BondsRestoring/" ++ moleculeName ++ ".mae"
      beforeAll ((,) <$> pdbIO <*> maeIO) $ do
        it (moleculeName ++ " equal global bond count in Mae and PDB") $ \(modelFromPDB, modelFromMae) -> do
            let (pdbGlobalBondCount, _, _, _) = getStats modelFromPDB
            let (maeGlobalBondCount, _, _, _) = getStats modelFromMae
            pdbGlobalBondCount `shouldBe` maeGlobalBondCount
        it (moleculeName ++ " equal local bond count in Mae and PDB") $ \(modelFromPDB, modelFromMae) -> do
            let (_, pdbLocalBondCount, _, _) = getStats modelFromPDB
            let (_, maeLocalBondCount, _, _) = getStats modelFromMae
            pdbLocalBondCount `shouldBe` maeLocalBondCount

        it (moleculeName ++ " no dublicate bonds") $ \(modelFromPDB, _) ->
          length (doubleBonds modelFromPDB) `shouldBe` 0

        it (moleculeName ++ " difference in Mae and PDB global bond sets") $ \(modelFromPDB, modelFromMae) -> do
            let _globalBondSetPDB = globalBondSet modelFromPDB
            let _globalBondSetMae = globalBondSet modelFromMae
            let diffPDBMaeGlobal = S.difference _globalBondSetPDB _globalBondSetMae
            let diffMaePDBGlobal = S.difference _globalBondSetMae _globalBondSetPDB
            S.size diffPDBMaeGlobal `shouldBe` 0
            S.size diffMaePDBGlobal `shouldBe` 0

        it (moleculeName ++ " difference in Mae and PDB local bond sets") $ \(modelFromPDB, modelFromMae) -> do
            let _localBondSetPDB = localBondSet modelFromPDB
            let _localBondSetMae = localBondSet modelFromMae
            let diffMaePDBLocal = S.difference _localBondSetMae _localBondSetPDB
            let diffPDBMaeLocal = S.difference _localBondSetPDB _localBondSetMae
            S.size diffMaePDBLocal `shouldBe` 0
            S.size diffPDBMaeLocal `shouldBe` 0

    localBondSet :: Model -> Set (Text, Int, Int, Int) -- (ChainID, ResidueNumber, LocalFrom, LocalTo)
    localBondSet Model{..} = S.fromList $ do
      Chain{..} <- V.toList modelChains
      Residue{..} <- V.toList chainResidues
      Bond (LocalID from) (LocalID to) _ <- V.toList resBonds

      [(chainName, resNumber, from, to), (chainName, resNumber, to, from)]

    doubleBonds :: Model -> [Bond GlobalID]
    doubleBonds Model{..} = doubleBonds' (V.toList modelBonds) []
      where
        doubleBonds' :: [Bond GlobalID] -> [Bond GlobalID] -> [Bond GlobalID]
        doubleBonds' [] acc = acc
        doubleBonds' (b:bs) acc = doubleBonds' bs . maybe acc (:acc) $ find (bondsEqual b) bs
        bondsEqual :: Bond GlobalID -> Bond GlobalID -> Bool
        bondsEqual b1 b2 = bondStart b1 == bondStart b2 && bondEnd b1 == bondEnd b2

    globalBondSet :: Model -> Set (Text,Text)
    globalBondSet Model{..} = bondSet getGlobalID (chainsAtomMap modelChains) modelBonds

    chainsAtomMap :: Vector Chain -> Map Int (Text, Atom)
    chainsAtomMap chains = M.fromList $ concatMap chainAtomPreMap chains

    chainAtomPreMap :: Chain -> [(Int, (Text, Atom))]
    chainAtomPreMap Chain{..} = V.toList . fmap (\a -> (getGlobalID $ atomId a, (chainName, a))) $ V.concatMap resAtoms chainResidues

    bondSet :: (a -> Int) -> Map Int (Text, Atom) -> Vector (Bond a) -> Set (Text,Text)
    bondSet getID atomMap bonds = S.fromList $ do
      Bond{..} <- V.toList bonds
      let atomFromId = formAtomId $ atomMap ! getID bondStart
      let atomToId = formAtomId $ atomMap ! getID bondEnd
      [(atomFromId, atomToId), (atomToId, atomFromId)]
      where
        formAtomId :: (Text, Atom) -> Text
        formAtomId (chainId, Atom{..}) = chainId <> "_" <> atomName <> "_" <> T.pack (show $ getGlobalID atomId)


getStats :: Model -> (Int, Int, Int, Int)
getStats model = (globalBondCount, localBondCount, chainCount, atomCount)
  where
    globalBondCount = V.length $ modelBonds model
    localBondCount = V.length . V.concatMap (V.concatMap resBonds . chainResidues) $ modelChains model
    chainCount = V.length $ modelChains model
    atomCount = V.length . V.concatMap (V.concatMap resAtoms . chainResidues) $ modelChains model

ef :: NFData a => a -> IO a
ef = evaluate . force

firstPDBModel :: (MonadIO m) => FilePath -> m Model
firstPDBModel filepath = do
  eitherPDB <- modelsFromPDBFile filepath
  let (_, models) = fromRight undefined eitherPDB
  liftIO . ef $ V.head models

firstMaeModel :: (MonadIO m) => FilePath -> m Model
firstMaeModel filepath = do
  eitherMae <- modelsFromMaeFile filepath
  liftIO . ef . V.head $ fromRight undefined eitherMae
