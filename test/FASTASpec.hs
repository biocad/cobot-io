{-# LANGUAGE OverloadedStrings #-}

module FASTASpec where

import           Bio.FASTA              (fastaP, fromFile, toFile)
import           Bio.FASTA.Type         (Fasta, FastaItem (..))
import           Bio.Sequence           (bareSequence)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bifunctor
import           Data.Text              (Text)
import           Data.Text.IO           (readFile)
import           Data.Void              (Void)
import           Prelude                hiding (readFile, writeFile)
import           System.Directory       (removeFile)
import           Test.Hspec
import           Text.Megaparsec        (MonadParsec (eof), Parsec,
                                         errorBundlePretty, parse)

parseOnly :: Parsec Void Text (Fasta a) -> Text -> Either String (Fasta a)
parseOnly p s = first errorBundlePretty $ parse (p <* eof) "test.fasta" s

correctFasta1 :: Fasta Char
correctFasta1 = [ FastaItem "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
                , FastaItem "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
                , FastaItem "With_spaces" (bareSequence "MDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRERMDFFDLDIEIKQERLPAECSLNSPLNYSLSAQLTDRMTPRTENVRRQRER")
                , FastaItem "Empty_ha_ha_ha" (bareSequence "")
                ]

correctFasta3 :: Fasta Char
correctFasta3 = [ FastaItem "N-His-E4Orf6-7-R2(115)" (bareSequence "TGATGGTGATGGTGATGcatGTGGTAAACTCGACTTTCACTTTTCTCTATCACTGATAGGGAGTGGTAAACTCGACTTTCACTTTTCTCTATCACTGATAGGGAaacagtcagcc")
                ]

badFasta4 :: Either String (Fasta Char)
badFasta4 = Left "test.fasta:5:1:\n  |\n5 | HindIII-BFP_F                \r\n  | ^\nunexpected 'H'\nexpecting '>' or end of input\n"

badFasta5 :: Either String (Fasta Char)
badFasta5 = Left "test.fasta:8:1:\n  |\n8 | qCHO R \r\n  | ^\nunexpected 'q'\nexpecting '>' or end of input\n"

badFasta6 :: Either String (Fasta Char)
badFasta6 = Left "test.fasta:22:1:\n   |\n22 | sPA-LoxP-NheI_R           \r\n   | ^\nunexpected 's'\nexpecting '>' or end of input\n"

badFasta7 :: Either String (Fasta Char)
badFasta7 = Left "test.fasta:2:1:\n  |\n2 | 5\8217-CTTCAAGAGAGAGACCTGCGT-3\8217\r\n  | ^\nunexpected '5'\nexpecting '>', end of input, or sequence\n"

badFasta8 :: Either String (Fasta Char)
badFasta8 = Left "test.fasta:21:5:\n   |\n21 | CMV + enhMCK + prcTnT-2\r\n   |     ^^\nunexpected \"+ \"\nexpecting end of input, end of line, or letter\n"

fastaSpec :: Spec
fastaSpec = describe "Fasta files parser." $ do
    parseFile "test/FASTA/order1.fasta" correctFasta1
    writeFile "test/FASTA/test.fasta" correctFasta1
    parseFile "test/FASTA/order3.fasta" correctFasta3
    writeFile "test/FASTA/test.fasta" correctFasta3
    parseBadFile "test/FASTA/order4.fasta" badFasta4
    parseBadFile "test/FASTA/order5.fasta" badFasta5
    parseBadFile "test/FASTA/order6.fasta" badFasta6
    parseBadFile "test/FASTA/order7.fasta" badFasta7
    parseBadFile "test/FASTA/Ампликон_28_07_22.FASTA" badFasta8

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
