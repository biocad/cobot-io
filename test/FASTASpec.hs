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

correctTest1 :: Text
correctTest1 = T.unlines
  [ ">test1"
  , "ABCDEF"
  , "GHIJKL"
  , ""
  , ">test2"
  , "ABCDEF"
  ]

correctTest2 :: Text
correctTest2 = T.unlines
  [ ">test1"
  , "ABCDEF"
  , "GHIJKL"
  , " "
  , ">test2"
  , "ABCDEF"
  ]

correctTest3 :: Text
correctTest3 = T.unlines
  [ ">test1"
  , "ABCDEF"
  , "GHIJKL"
  , "\t"
  , ">test2"
  , "ABCDEF"
  ]

incorrectTest1 :: Text
incorrectTest1 = T.unlines
  [ "test1"
  , "ABCDEF"
  , "GHIJKL"
  , ">test2"
  , "ABCDEF"
  ]

incorrectTest2 :: Text
incorrectTest2 = T.unlines
  [ ">test1"
  , "ABCDEF"
  , "GHIJKL >test2"
  , "ABCDEF"
  ]

correctAnswer :: Fasta Char
correctAnswer = [FastaItem "test1" (bareSequence "ABCDEFGHIJKL"), FastaItem "test2" (bareSequence "ABCDEF")]

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
    describe "various parser tests" $ do
        it "correctly parses empty lines" $ checkParser correctTest1 (Right correctAnswer)
        it "correctly parses empty lines with spaces" $ checkParser correctTest2 (Right correctAnswer)
        it "correctly parses empty lines with tabs" $ checkParser correctTest3 (Right correctAnswer)
        it "correctly fails to parse a name without >" $ checkParser incorrectTest1 (Left "endOfInput")
        it "correctly fails to parse a new sequence at the same line" $ checkParser incorrectTest2 (Left "endOfInput")


writeFile :: FilePath -> Spec
writeFile path = describe "writeFile" $ do
    it "correctly write fasta into file" $ do
        toFile correctFasta path
        fasta <- fromFile path
        removeFile path
        fasta `shouldBe` correctFasta

checkParser :: Text -> Either String (Fasta Char) -> Expectation
checkParser source expectation = parseOnly (fastaP <* endOfInput) source `shouldBe` expectation
