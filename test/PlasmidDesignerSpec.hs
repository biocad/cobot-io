{-# LANGUAGE OverloadedStrings #-}

module PlasmidDesignerSpec where

import           Bio.GB.PlasmidDesigner (updateGB)
import           Bio.GB       (Feature (..), Form (..), GenBankSequence (..),
                               Locus (..), Meta (..), Reference (..),
                               Source (..), Version (..), fromFile, PlasmidFormat(..))
import           Bio.FASTA.Type (FastaItem(..))
import           Bio.Sequence           (bareSequence, unsafeMarkedSequence)
import           Data.Attoparsec.Text   (parseOnly)
import           Bio.Sequence (Range)
import           Test.Hspec

plasmidDesignerSpec :: Spec
plasmidDesignerSpec = describe "Plasmid designer" $
    beforeWith readFormat $ do
--        emptyFormat
--        emptyFasta
--        emptyFormatStuffer
--        emptyFastaSequ
        sequTheSameLengthWithStuffer
        sequWithBiggestLengthThanStuffer
        sequWithLessLengthThanStuffer
        sequInTheEndOfPlasmidTheSameLengthWithStuffer
        sequInTheEndOfPlasmidBiggestLengthThanStuffer
        sequInTheEndOfPlasmidLessLengthThanStuffer

readFormat :: () -> IO GenBankSequence
readFormat _ = do
    plasmid <- fromFile "test/GB/BCD216.gb"
    return (plasmid)

--emptyFormatStuffer :: SpecWith GenBankSequence
--emptyFormatStuffer = describe "emptyFormatStuffer" $ do
--    it "when format stuffer is empty should be error" $ \plasmid -> do
--
--        let format = PlasmidFormat plasmid "UNKNOWN_STUFFER"
--        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "ACG")
--
--        let res = updateGB format fastaItem
--        res `shouldBe` sample

--emptyFastaSequ :: SpecWith GenBankSequence
--emptyFastaSequ = describe "emptyFastaSequ" $ do
--    it "when new sequence is empty should be error" $ \plasmid -> do
--
--        let format = PlasmidFormat plasmid "BCD216-00_ABVH_000_01"
--        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "")
--
--        let res = updateGB format fastaItem
--        res `shouldBe` sample

sequTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequTheSameLengthWithStuffer = describe "sequTheSameLengthWithStuffer" $ do
    it "when new sequence has the same length as stuffer" $ \plasmid -> do

        let format = PlasmidFormat plasmid "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAAT")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_thesame_len.gb"
        res `shouldBe` sample

sequWithBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequWithBiggestLengthThanStuffer = describe "sequWithBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length as stuffer" $ \plasmid -> do

        let format = PlasmidFormat plasmid "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCCGGAATTCG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_biggest_len.gb"
        res `shouldBe` sample

sequWithLessLengthThanStuffer :: SpecWith GenBankSequence
sequWithLessLengthThanStuffer = describe "sequWithLessLengthThanStuffer" $ do
    it "when new sequence has less  length than stuffer" $ \plasmid -> do

        let format = PlasmidFormat plasmid "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_less_len.gb"
        res `shouldBe` sample

sequInTheEndOfPlasmidTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidTheSameLengthWithStuffer = describe "sequInTheEndOfPlasmidTheSameLengthWithStuffer" $ do
    it "when new sequence the same length as stuffer and element located in the end of plasmid" $ \plasmid -> do

        let format = PlasmidFormat plasmid "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_thesame_len.gb"
        res `shouldBe` sample


sequInTheEndOfPlasmidBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidBiggestLengthThanStuffer = describe "sequInTheEndOfPlasmidBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length than stuffer and element located in the end of plasmid" $ \plasmid -> do

        let format = PlasmidFormat plasmid "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGGAA")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_biggest_len.gb"
        res `shouldBe` sample

sequInTheEndOfPlasmidLessLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidLessLengthThanStuffer = describe "sequInTheEndOfPlasmidLessLengthThanStuffer" $ do
    it "when new sequence has less length than stuffer and element located in the end of plasmid" $ \plasmid -> do

        let format = PlasmidFormat plasmid "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_less_len.gb"
        res `shouldBe` sample

