{-# LANGUAGE OverloadedStrings #-}

module PDBSpec where

import           Bio.PDB       (modelsFromPDBFile)
import           Bio.Structure (Model (..), Chain(..), Residue(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either (fromRight)
import qualified Data.Vector as V (head, length, toList, concatMap)
import           Test.Hspec

-- TODO: write tests to validate bond number on tripeptides

rawPDBToModelConversionSpec = describe "Cobot Model from raw PDB" $ do
  model <- runIO $ firstModel "test/PDB/1PPE_I.pdb"
  let bondCount = V.length $ modelBonds model
  let chainCount = V.length $ modelChains model
  let atomCount = V.length . V.concatMap (V.concatMap resAtoms . chainResidues) $ modelChains model

  it "should have correct number of atoms" $ atomCount `shouldBe` 436
  it "should have correct number of chains" $ chainCount `shouldBe` 1
  it "should restore bonds correctly" $ bondCount `shouldBe` 441

firstModel :: (MonadIO m) => FilePath -> m Model
firstModel filepath = do
  eitherPDB <- modelsFromPDBFile filepath
  let (_, models) = fromRight undefined eitherPDB

  pure $ V.head models
