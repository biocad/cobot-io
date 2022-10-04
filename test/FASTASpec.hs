{-# LANGUAGE OverloadedStrings #-}

module FASTASpec where

import Bio.FASTA              (fastaP, fromFile, toFile)
import Bio.FASTA.Parser       (parseOnly)
import Bio.FASTA.Type         (Fasta, FastaItem (..))
import Bio.Sequence           (bareSequence)
import Control.Monad.IO.Class (liftIO)
import Data.Text.IO           (readFile)
import Prelude                hiding (readFile, writeFile)
import System.Directory       (removeFile)
import Test.Hspec

correctFasta1 :: Fasta Char
correctFasta1 = [ FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
                , FastaItem "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
                , FastaItem "With_spaces" (bareSequence "MDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRER")
                , FastaItem "Empty_ha_ha_ha" (bareSequence "")
                ]

badFasta2 :: Either String (Fasta Char)
badFasta2 = Left "input.fasta:2:5:\n  |\n2 | ACGT....TCG\r\n  |     ^^\nunexpected \"..\"\nexpecting end of input, end of line, or letter\n"


correctFasta3 :: Fasta Char
correctFasta3 = [ FastaItem "N-His-E4Orf6-7-R2(115)" (bareSequence "TGATGGTGATGGTGATGcatGTGGTAAACTCGACTTTCACTTTTCTCTATCACTGATAGGGAGTGGTAAACTCGACTTTCACTTTTCTCTATCACTGATAGGGAaacagtcagcc")
                ]

badFasta4 :: Either String (Fasta Char)
badFasta4 = Left "input.fasta:5:8:\n  |\n5 | HindIII-BFP_F                \r\n  |        ^^\nunexpected \"-B\"\nexpecting end of input, end of line, or letter\n"

correctFasta5 :: Fasta Char
correctFasta5 = [FastaItem "qCHO49 F" (bareSequence "TGGAGAGATGGCTCGAGGTTqCHORTGGTTGCTGGGAATTGAACTC")]

badFasta6 :: Either String (Fasta Char)
badFasta6 = Left "input.fasta:22:1:\n   |\n22 | sPA-LoxP-NheI_R           \r\n   | ^\nunexpected 's'\nexpecting '>' or end of input\n"

badFasta7 :: Either String (Fasta Char)
badFasta7 = Left "input.fasta:2:1:\n  |\n2 | 5\8217-CTTCAAGAGAGAGACCTGCGT-3\8217\r\n  | ^\nunexpected '5'\nexpecting '>', end of input, end of line, or sequence\n"

badFasta8 :: Either String (Fasta Char)
badFasta8 = Left "input.fasta:21:5:\n   |\n21 | CMV + enhMCK + prcTnT-2\r\n   |     ^^\nunexpected \"+ \"\nexpecting end of input, end of line, or letter\n"

fastaSpec :: Spec
fastaSpec = describe "Fasta files parser." $ do
    parseFile "test/FASTA/order1.fasta" correctFasta1
    writeFile "test/FASTA/input.fasta" correctFasta1
    parseBadFile "test/FASTA/order2.fasta" badFasta2
    parseFile "test/FASTA/order3.fasta" correctFasta3
    writeFile "test/FASTA/input.fasta" correctFasta3
    parseBadFile "test/FASTA/order4.fasta" badFasta4
    parseFile  "test/FASTA/order5.fasta" correctFasta5
    writeFile "test/FASTA/input.fasta" correctFasta5
    parseBadFile "test/FASTA/order6.fasta" badFasta6
    parseBadFile "test/FASTA/order7.fasta" badFasta7
    parseBadFile "test/FASTA/order8.fasta" badFasta8

parseFile :: FilePath -> Fasta Char -> Spec
parseFile path cf = do
    describe "fromFile" $ do
        it "correctly parses fasta from file" $ do
            fasta <- fromFile path
            fasta `shouldBe` cf

parseBadFile :: FilePath -> Either String (Fasta Char) -> Spec
parseBadFile path cf = do
    describe "fromFile" $ do
         it "correctly parses fasta from file" $ do
            res <- liftIO (readFile path)
            let badRes = parseOnly fastaP res
            badRes `shouldBe` cf

writeFile :: FilePath -> Fasta Char -> Spec
writeFile path cf = describe "writeFile" $ do
    it "correctly write fasta into file" $ do
        toFile cf path
        fasta <- fromFile path
        removeFile path
        fasta `shouldBe` cf
