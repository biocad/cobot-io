{-# LANGUAGE OverloadedStrings #-}

module FASTASpec where

import           Bio.FASTA            (fastaP, fromFile, toFile)
import           Bio.FASTA.Type       (Fasta, FastaItem (..))
import           Bio.Sequence         (bareSequence)
import           Data.Attoparsec.Text (endOfInput, parseOnly)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding (readFile, writeFile)
import           System.Directory     (removeFile)
import           Test.Hspec

correctFasta :: Fasta Char
correctFasta = [ FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
               , FastaItem "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
               , FastaItem "With_spaces" (bareSequence "MDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRER")
               , FastaItem "Empty_ha_ha_ha" (bareSequence "")
               ]

fastaSpec :: Spec
fastaSpec = describe "Fasta file parser." $ do
    parseFile "test/FASTA/correct.fasta"
    writeFile "test/FASTA/test.fasta"

parseFile :: FilePath -> Spec
parseFile path = do
    describe "fromFile" $ do
        it "correctly parses fasta from file" $ do
            fasta <- fromFile path
            fasta `shouldBe` correctFasta

writeFile :: FilePath -> Spec
writeFile path = describe "writeFile" $ do
    it "correctly write fasta into file" $ do
        toFile correctFasta path
        fasta <- fromFile path
        removeFile path
        fasta `shouldBe` correctFasta

