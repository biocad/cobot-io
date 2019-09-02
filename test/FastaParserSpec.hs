{-# LANGUAGE OverloadedStrings #-}

module FastaParserSpec where

import           Test.Hspec
import           Bio.FASTA.Parser       (fastaP)
import           Data.Attoparsec.Text   (parseOnly)
import           Bio.Sequence           (bareSequence)
import           Bio.FASTA.Type         (FastaItem(..))

fastaParserSpec :: Spec
fastaParserSpec = describe "Fasta format parser." $ do
    emptyFasta
    oneSequence
    twoSequences
    sequenceWithDigit
    sequenceWithWrongName
    sequenceWithSpacesInName
    sequenceWithSeveralEndOfLine
    sequenceWithSeveralEndOfLineInSequence
    sequenceWithTabsInName
    sequenceWithTabsInSequence

emptyFasta :: Spec
emptyFasta = describe "emptyFasta" $ do
    it "correctly parses empty fasta" $ do
        let res = parseOnly fastaP ""
        res `shouldBe` Right []

oneSequence :: Spec
oneSequence = describe "oneSequence" $ do
    it "correctly parses one correct sequence" $ do
        let res = parseOnly fastaP ">3HMX:A|PDBID|CHAIN|SEQUENCE\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL\n"
        res `shouldBe` Right [FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")]

twoSequences :: Spec
twoSequences = describe "twoSequences" $ do
    it "correctly parses two correct sequences" $ do
        let res = parseOnly fastaP ">3HMX:A|PDBID|CHAIN|SEQUENCE\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL\n>7HMX:A|PDBID|CHAIN|SEQUENCE\nEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL"
        res `shouldBe` Right [FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL"), FastaItem "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")]

sequenceWithDigit :: Spec
sequenceWithDigit = describe "sequenceWithDigit" $ do
    it "correctly parses incorrect sequence with digit" $ do
        let res = parseOnly fastaP ">123\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEE4GITWTLDQSSE"
        res `shouldBe` Right []

sequenceWithWrongName :: Spec
sequenceWithWrongName = describe "sequenceWithWrongName" $ do
    it "correctly parses incorrect sequence with wrong name" $ do
        let res = parseOnly fastaP "123\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right []

sequenceWithSpacesInName :: Spec
sequenceWithSpacesInName = describe "sequenceWithSpacesInName" $ do
    it "correctly parses sequence with spaces in name" $ do
        let res = parseOnly fastaP ">  this is my sequence   \nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem "  this is my sequence   " (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithSeveralEndOfLine :: Spec
sequenceWithSeveralEndOfLine = describe "sequenceWithSeveralEndOfLine" $ do
    it "correctly parses sequence with several \n after name" $ do
        let res = parseOnly fastaP ">this is my sequence\n\n\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithSeveralEndOfLineInSequence :: Spec
sequenceWithSeveralEndOfLineInSequence = describe "sequenceWithSeveralEndOfLineInSequence" $ do
    it "correctly parses sequence with several \n between sequence parts" $ do
        let res = parseOnly fastaP ">this is my sequence\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE\n\n\nYYYYYYYYYYYYYYYYYYYYYYYY"
        res `shouldBe` Right [FastaItem "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSEYYYYYYYYYYYYYYYYYYYYYYYY")]

sequenceWithTabsInName :: Spec
sequenceWithTabsInName = describe "sequenceWithTabsInName" $ do
    it "correctly parses sequence with tabs in name" $ do
        let res = parseOnly fastaP ">this is my sequence\t\t\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithTabsInSequence :: Spec
sequenceWithTabsInSequence = describe "sequenceWithTabsInSequence" $ do
    it "correctly parses sequence with tabs between sequence parts" $ do
        let res = parseOnly fastaP ">this is my sequence\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE\t\t\nYYYYYYYYYYYYYYYYYYYYYYYY\t\n"
        res `shouldBe` Right [FastaItem "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSEYYYYYYYYYYYYYYYYYYYYYYYY")]