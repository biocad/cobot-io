{-# LANGUAGE OverloadedStrings #-}

module PlasmidDesignerSpec where

import           Bio.FASTA.Type         (FastaItem (..))
import           Bio.GB                 (GenBankSequence (..),
                                         PlasmidFormat (..),fromFile)
import           Bio.GB.PlasmidDesigner (updateGB)
import           Bio.Sequence           (bareSequence)
import           Test.Hspec

plasmidDesignerSpec :: Spec
plasmidDesignerSpec = describe "Plasmid designer" $
    beforeAll readFormat $ do
        sequTheSameLengthWithStuffer
        sequWithBiggestLengthThanStuffer
        sequWithLessLengthThanStuffer
        sequInTheEndOfPlasmidTheSameLengthWithStuffer
        sequInTheEndOfPlasmidBiggestLengthThanStuffer
        sequInTheEndOfPlasmidLessLengthThanStuffer

readFormat :: IO GenBankSequence
readFormat = do
    plasmidSeq <- fromFile "test/GB/BCD216.gb"
    return (plasmidSeq)

sequTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequTheSameLengthWithStuffer = describe "sequTheSameLengthWithStuffer" $ do
    it "when new sequence has the same length as stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAAT")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_thesame_len.gb"
        res `shouldBe` sample

sequWithBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequWithBiggestLengthThanStuffer = describe "sequWithBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length as stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCCGGAATTCG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_biggest_len.gb"
        res `shouldBe` sample

sequWithLessLengthThanStuffer :: SpecWith GenBankSequence
sequWithLessLengthThanStuffer = describe "sequWithLessLengthThanStuffer" $ do
    it "when new sequence has less  length than stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_less_len.gb"
        res `shouldBe` sample

sequInTheEndOfPlasmidTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidTheSameLengthWithStuffer = describe "sequInTheEndOfPlasmidTheSameLengthWithStuffer" $ do
    it "when new sequence the same length as stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_thesame_len.gb"
        res `shouldBe` sample


sequInTheEndOfPlasmidBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidBiggestLengthThanStuffer = describe "sequInTheEndOfPlasmidBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length than stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGGAA")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_biggest_len.gb"
        res `shouldBe` sample

sequInTheEndOfPlasmidLessLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidLessLengthThanStuffer = describe "sequInTheEndOfPlasmidLessLengthThanStuffer" $ do
    it "when new sequence has less length than stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_less_len.gb"
        res `shouldBe` sample

