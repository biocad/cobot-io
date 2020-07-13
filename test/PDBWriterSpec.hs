{-# LANGUAGE OverloadedStrings #-}

module PDBWriterSpec where

import           Bio.PDB                (modelsFromPDBFile, modelsToPDBText)
import           Control.Monad          (zipWithM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO (readFile)
import           Test.Hspec

pdbWriterSpec :: Spec
pdbWriterSpec = describe "PDB format writer." $ do
    justWritingSpec
    severalModelsSpec "writing"
    hetatmSpec "writing"

justWritingSpec :: Spec
justWritingSpec = it "Correctly writes very big PDB." $ do
    Right (_, models) <- modelsFromPDBFile testFile
    modelsText        <- liftIO $ TIO.readFile testFile

    let modelsTextFromWriter = modelsToPDBText models

    zipWithM_ shouldBe (T.splitOn newLine modelsTextFromWriter) $ T.splitOn newLine modelsText
  where
    testFile = "test/PDB/Writer/big_file.pdb"

severalModelsSpec :: FilePath -> Spec
severalModelsSpec path = describe "Correctly writes several models." $ do
    pure ()

hetatmSpec :: FilePath -> Spec
hetatmSpec path = describe "Correctly wrtes models with HETATMs." $ do
  pure ()

newLine :: Text
newLine = "\n"
