{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MAESpec where

import           Bio.MAE       (fromFile)
import           Bio.Structure (Atom (..), Bond (..), Chain (..), GlobalID (..),
                                LocalID (..), Model (..), Residue (..),
                                SecondaryStructure (..), StructureModels (..),
                                StructureSerializable (..))
import qualified Data.Vector   as V (toList)
import           Test.Hspec

maeSpec :: Spec
maeSpec = describe "Mae spec." $ do
    Model{..} <- runIO $ V.toList . modelsOf <$> fromFile "test/MAE/small.mae" >>= \[x] -> pure x

    it "one model" $ length modelChains `shouldBe` 1

parseFileSpec :: FilePath -> Expectation
parseFileSpec path = fromFile path >> pure ()
