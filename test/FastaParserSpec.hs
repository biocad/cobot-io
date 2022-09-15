{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FastaParserSpec where

import           Bio.FASTA.Parser (fastaP)
import           Bio.FASTA.Type   (Fasta, FastaItem (..), ModItem (..), Modification (..))
import           Bio.Sequence     (bareSequence)
import           Data.Bifunctor
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Void        (Void)
import           Test.Hspec
import           Text.Megaparsec  (Parsec, eof, errorBundlePretty, parse)

fastaParserSpec :: Spec
fastaParserSpec = describe "Fasta format parser" $ do
    emptyFasta
    onlyName
    oneSequence
    twoSequences
    sequenceWithDigit
    sequenceWithWrongName
    sequenceWithSpacesInName
    sequenceWithSeveralEndOfLine
    sequenceWithSeveralEndOfLineInSequence
    sequenceWithTabsInName
    sequenceWithTabsInSequence
    sequenceWithModifications
    sequenceWithSpaces
    toughParserTests

parseOnly :: Parsec Void Text (Fasta a) -> Text -> Either String (Fasta a)
parseOnly p s = first errorBundlePretty $ parse (p <* eof) "test.fasta" s

emptyFasta :: Spec
emptyFasta = describe "emptyFasta" $ do
    it "correctly parses empty fasta" $ do
        let res = parseOnly fastaP ""
        res `shouldBe` Right ([] :: Fasta Char)

onlyName :: Spec
onlyName = describe "onlyName" $ do
    it "correctly parses fasta without sequence" $ do
        let res = parseOnly fastaP ">3HMX:A|PDBID|CHAIN|SEQUENCE"
        res `shouldBe` Right [FastaItem @Char "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "")]

oneSequence :: Spec
oneSequence = describe "oneSequence" $ do
    it "correctly parses one correct sequence" $ do
        let res = parseOnly fastaP ">3HMX:A|PDBID|CHAIN|SEQUENCE\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL\n"
        res `shouldBe` Right [FastaItem @Char "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")]

twoSequences :: Spec
twoSequences = describe "twoSequences" $ do
    it "correctly parses two correct sequences" $ do
        let res = parseOnly fastaP ">3HMX:A|PDBID|CHAIN|SEQUENCE\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL\n>7HMX:A|PDBID|CHAIN|SEQUENCE\nEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE\nVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL"
        res `shouldBe` Right
          [ FastaItem @Char "3HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEDGITWTLDQSSEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
          , FastaItem @Char "7HMX:A|PDBID|CHAIN|SEQUENCE" (bareSequence "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEVLGSGKTLTIQVKEFGDAGQYTCHKGGEVLSHSLL")
          ]

sequenceWithDigit :: Spec
sequenceWithDigit = describe "sequenceWithDigit" $ do
    it "correctly parses incorrect sequence with digit" $ do
        let res = parseOnly fastaP ">123\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEE4GITWTLDQSSE"
        res `shouldBe` Right [FastaItem @Char "123" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEE4GITWTLDQSSE")]

sequenceWithWrongName :: Spec
sequenceWithWrongName = describe "sequenceWithWrongName" $ do
    it "correctly parses incorrect sequence with wrong name" $ do
        let res = parseOnly (fastaP @Char) "123\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Left "test.fasta:1:1:\n  |\n1 | 123\n  | ^\nunexpected '1'\nexpecting '>' or end of input\n"

sequenceWithSpacesInName :: Spec
sequenceWithSpacesInName = describe "sequenceWithSpacesInName" $ do
    it "correctly parses sequence with spaces in name" $ do
        let res = parseOnly fastaP ">  this is my sequence   \nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem @Char "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithSeveralEndOfLine :: Spec
sequenceWithSeveralEndOfLine = describe "sequenceWithSeveralEndOfLine" $ do
    it "correctly parses sequence with several \\n after name" $ do
        let res = parseOnly fastaP ">this is my sequence\n\n\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem @Char "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithSeveralEndOfLineInSequence :: Spec
sequenceWithSeveralEndOfLineInSequence = describe "sequenceWithSeveralEndOfLineInSequence" $ do
    it "correctly parses sequence with several \\n between sequence parts" $ do
        let res = parseOnly fastaP ">this is my sequence\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE\n\n\nYYYYYYYYYYYYYYYYYYYYYYYY"
        res `shouldBe` Right [FastaItem @Char "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSEYYYYYYYYYYYYYYYYYYYYYYYY")]

sequenceWithTabsInName :: Spec
sequenceWithTabsInName = describe "sequenceWithTabsInName" $ do
    it "correctly parses sequence with tabs in name" $ do
        let res = parseOnly fastaP ">\tthis\tis\tmy\tsequence\t\t\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE"
        res `shouldBe` Right [FastaItem @Char "this\tis\tmy\tsequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE")]

sequenceWithTabsInSequence :: Spec
sequenceWithTabsInSequence = describe "sequenceWithTabsInSequence" $ do
    it "correctly parses sequence with tabs between sequence parts" $ do
        let res = parseOnly fastaP ">this is my sequence\nIWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSE\t\t\nYYYYYYYYYYYYYYYYYYYYYYYY\t\n"
        res `shouldBe` Right [FastaItem @Char "this is my sequence" (bareSequence "IWELKKDVYVVELDWYPDAPGEMVVLTCDTPEEGITWTLDQSSEYYYYYYYYYYYYYYYYYYYYYYYY")]

sequenceWithModifications :: Spec
sequenceWithModifications = describe "sequenceWithModifications" $ do
    it "correctly parses sequence with modifications" $ do
        let res = parseOnly fastaP ">this is my sequence\nIWEL[mU*]KKDVYV\t\t\nYY[56FAM]YY[Trololo]YY\t\n"
        res `shouldBe` Right [FastaItem "this is my sequence" (bareSequence [Letter 'I', Letter 'W', Letter 'E', Letter 'L', Mod Mod_mU_Star, Letter 'K', Letter 'K', Letter 'D', Letter 'V', Letter 'Y', Letter 'V', Letter 'Y', Letter 'Y', Mod Mod_56FAM, Letter 'Y', Letter 'Y', Mod (Unknown "[Trololo]"), Letter 'Y', Letter 'Y'])]

sequenceWithSpaces :: Spec
sequenceWithSpaces = describe "sequenceWithSpaces" $ do
  it "correctly parses sequence with spaces" $ do
      let res = parseOnly fastaP ">test1\nAAAA TTTT GGGG ccA\n"
      res `shouldBe` Right [FastaItem @Char "test1" (bareSequence "AAAATTTTGGGGccA")]

toughParserTests :: Spec
toughParserTests = describe "various parser tests" $ do
    it "correctly parses empty lines" $ checkParser correctTest1 (Right correctAnswer)
    it "correctly parses empty lines with spaces" $ checkParser correctTest2 (Right correctAnswer)
    it "correctly parses empty lines with tabs" $ checkParser correctTest3 (Right correctAnswer)
    it "correctly parses empty lines with trailing tabs" $ checkParser correctTest4 (Right correctAnswer4)
    it "correctly fails to parse a name without >" $ checkParser incorrectTest1
      (Left "test.fasta:1:1:\n  |\n1 | test1\n  | ^\nunexpected 't'\nexpecting '>' or end of input\n")
    it "correctly fails to parse a new sequence at the same line" $ checkParser incorrectTest2
      (Left "test.fasta:3:8:\n  |\n3 | GHIJKL >test2\n  |        ^^\nunexpected \">t\"\nexpecting alphanumeric character, end of input, or end of line\n")

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

correctTest4 :: Text
correctTest4 = "> test4\nTTTAGGTactTGT\t\t                                                                             \t\n"

correctAnswer4 :: [FastaItem Char]
correctAnswer4 =
  [ FastaItem "test4" (bareSequence "TTTAGGTactTGT")
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

checkParser :: HasCallStack => Text -> Either String (Fasta Char) -> Expectation
checkParser source expectation =
  first errorBundlePretty (parse (fastaP <* eof) "test.fasta" source)
    `shouldBe` expectation
