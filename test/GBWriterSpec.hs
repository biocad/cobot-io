{-# LANGUAGE OverloadedStrings #-}

module GBWriterSpec where

import           Bio.GB     (GenBankSequence (..), fromFile, fromText, toText)
import           Test.Hspec

gbWriterSpec :: Spec
gbWriterSpec = describe "GenBank format writer." $ do
      pAAVGFPSpecW "test/GB/pAAV-GFP-CellBioLab.gb"
      pAAVCMVSpecW "test/GB/pAAV_CMV_RPE65_PolyA_linkers.gb"
      dottedMetaSpecW "test/GB/pAAV-GFP-CellBioLab-dots.gb"

pAAVGFPSpecW :: FilePath -> Spec
pAAVGFPSpecW path = describe "pAAVGFP" $ do
    it "compares parsed structure with structure that is parsed from result of writing parsed structure" $ do
        st       <- fromFile path
        let text = toText st

        fromText text `shouldBe` Right st

pAAVCMVSpecW :: FilePath -> Spec
pAAVCMVSpecW path = describe "pAAVCMV" $ do
  it "compares parsed structure with structure that is parsed from result of writing parsed structure" $ do
      st       <- fromFile path
      let text = toText st

      fromText text `shouldBe` Right st

dottedMetaSpecW :: FilePath -> Spec
dottedMetaSpecW path = describe "Meta with dots." $ do
  it "compares parsed structure with structure that is parsed from result of writing parsed structure" $ do
      st       <- fromFile path
      let text = toText st

      meta <$> fromText text `shouldBe` Right (meta st)
