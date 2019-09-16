{-# LANGUAGE OverloadedStrings #-}

module PlasmidDesignerSpec where

import           Bio.FASTA.Type         (FastaItem (..))
import           Bio.GB                 (GenBankSequence (..),
                                         PlasmidFormat (..),fromFile)
import           Bio.GB.PlasmidDesigner (updateGB, DesignerError(..))
import           Bio.Sequence           (bareSequence, unsafeMarkedSequence)
import           Test.Hspec

import          Control.Monad.Except (MonadError)

plasmidDesignerSpec :: Spec
plasmidDesignerSpec = describe "Plasmid designer" $
    beforeAll readFormat $ do
        emptyFastaSequence
        emptyPlasmidSequence
        unknownStufferElement
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

emptyFastaSequence :: SpecWith GenBankSequence
emptyFastaSequence = describe "emptyFastaSequence" $ do
    it "should throw exception when fasta sequence is empty" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "")

        let res = updateGB format fastaItem
        res `shouldBe` Left (WrongArgumentsFormat "Empty fasta sequence")


emptyPlasmidSequence :: SpecWith GenBankSequence
emptyPlasmidSequence = describe "emptyPlasmidSequence" $ do
    it "should throw exception when plasmid sequence is empty" $ \(GenBankSequence meta plasmidSeq) -> do

        let format = PlasmidFormat (GenBankSequence meta (unsafeMarkedSequence [] [])) "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAAT")

        let res = updateGB format fastaItem
        res `shouldBe` Left (WrongArgumentsFormat "Empty plasmid sequence")


unknownStufferElement :: SpecWith GenBankSequence
unknownStufferElement = describe "unknownStufferElement" $ do
    it "should throw exception when stuffer does not exist in plasmid sequence" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "UNKNOWN"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAAT")

        let res = updateGB format fastaItem
        res `shouldBe` Left (NoSuchElement "There is no element with name UNKNOWN in plasmid")

sequTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequTheSameLengthWithStuffer = describe "sequTheSameLengthWithStuffer" $ do
    it "when new sequence has the same length as stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAAT")

        sample <- fromFile "test/GB/BCD216_replaced_thesame_len.gb"
        let res = updateGB format fastaItem
        res `shouldBe` Right sample

sequWithBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequWithBiggestLengthThanStuffer = describe "sequWithBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length as stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCCGGAATTCG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_biggest_len.gb"
        res `shouldBe` Right sample

sequWithLessLengthThanStuffer :: SpecWith GenBankSequence
sequWithLessLengthThanStuffer = describe "sequWithLessLengthThanStuffer" $ do
    it "when new sequence has less  length than stuffer" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "BCD216-00_ABVH_000_01"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_less_len.gb"
        res `shouldBe` Right sample

sequInTheEndOfPlasmidTheSameLengthWithStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidTheSameLengthWithStuffer = describe "sequInTheEndOfPlasmidTheSameLengthWithStuffer" $ do
    it "when new sequence the same length as stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_thesame_len.gb"
        res `shouldBe` Right sample


sequInTheEndOfPlasmidBiggestLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidBiggestLengthThanStuffer = describe "sequInTheEndOfPlasmidBiggestLengthThanStuffer" $ do
    it "when new sequence has biggest length than stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACGTCCTGGAA")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_biggest_len.gb"
        res `shouldBe` Right sample

sequInTheEndOfPlasmidLessLengthThanStuffer :: SpecWith GenBankSequence
sequInTheEndOfPlasmidLessLengthThanStuffer = describe "sequInTheEndOfPlasmidLessLengthThanStuffer" $ do
    it "when new sequence has less length than stuffer and element located in the end of plasmid" $ \plasmidSeq -> do

        let format = PlasmidFormat plasmidSeq "LAST_ELEMENT"
        let fastaItem = FastaItem "BCD216-REPLACEMENT" (bareSequence "GAAGTCCAATCGAACG")

        let res = updateGB format fastaItem
        sample <- fromFile "test/GB/BCD216_replaced_last_less_len.gb"
        res `shouldBe` Right sample

