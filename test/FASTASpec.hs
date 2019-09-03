{-# LANGUAGE OverloadedStrings #-}

module FASTASpec where

import           Bio.FASTA          (fromFile, toFile)
import           Bio.FASTA.Type     (FastaItem(..), Fasta)
import           Bio.Sequence       (bareSequence)
import           Prelude     hiding (writeFile, readFile)
import           Test.Hspec

correctFasta :: Fasta Char
correctFasta = [FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL"), FastaItem "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")]

fastaSpec :: Spec
fastaSpec = describe "Fasta file parser." $ do
    parseFile "test/FASTA/correct.fasta"
    writeFile "test/FASTA/test.fasta"

parseFile :: FilePath -> Spec
parseFile path = describe "fromFile" $ do
    it "correctly parses fasta from file" $ do
        fasta <- fromFile path
        fasta `shouldBe` correctFasta

writeFile :: FilePath -> Spec
writeFile path = describe "writeFile" $ do
    it "correctly write fasta into file" $ do
        toFile correctFasta path
        fasta <- fromFile path
        fasta `shouldBe` correctFasta
