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
    testWriting "correctly writes several models" "test/PDB/Writer/several_models.pdb"
    testWriting "correctly wrtes models with HETATMs" "test/PDB/Writer/hetatms.pdb"
    testWriting "correctly writes big complex model" "test/PDB/Writer/big_file.pdb"

testWriting :: String -> FilePath -> Spec
testWriting description path = it description $ do
    Right (_, models) <- modelsFromPDBFile path
    modelsText        <- liftIO $ TIO.readFile path

    let modelsTextFromWriter = modelsToPDBText models

    compareLinesOfText modelsTextFromWriter modelsText

compareLinesOfText :: Text -> Text -> Expectation
compareLinesOfText t = zipWithM_ shouldBe (T.splitOn newLine t) . T.splitOn newLine
  where
    newLine :: Text
    newLine = "\n"
