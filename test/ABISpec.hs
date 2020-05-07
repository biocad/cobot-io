{-# LANGUAGE RecordWildCards #-}

module ABISpec where

import           Data.ByteString.Lazy    as BSL (readFile)
import           Data.Text               (Text)
import           Test.Hspec

import           Bio.ABI                 (Cleanable (..))
import           Bio.ABI.Decode          (decodeRawSequence)
import           Bio.Sequence            (SequenceDecodable (..))
import qualified Bio.Sequence            as S (getWeights, length, toList)
import           Bio.Sequence.Basecalled (BasecalledSequence,
                                          BasecalledSequenceWithRawData (..))

abiExtractSpec :: Spec
abiExtractSpec =
  describe "ABI decode" $ do
    it "decode good ABI file" $ do
      Right dat <- readData "test/ABI/test.ab1"
      S.length dat     `shouldBe` 465
      S.toList dat     `shouldBe` goodSequence
      S.getWeights dat `shouldBe` goodQuality

    it "not decode non-ABI file" $ do
      datM <- readData "test/ABI/not_ab1.txt"
      datM `shouldBe` Left "Error reading root: not enough bytes"

    it "decode with raw data" $ do
      Right BasecalledSequenceWithRawData{..} <- decodeRawSequence <$> BSL.readFile "test/ABI/test.ab1"
      S.length bsSequence `shouldBe` length bsPeakLocations
      length bsRawG `shouldSatisfy` (>0)
      length bsRawA `shouldSatisfy` (>0)
      length bsRawT `shouldSatisfy` (>0)
      length bsRawC `shouldSatisfy` (>0)

abiCleanSpec :: Spec
abiCleanSpec =
  describe "ABI clean" $ do
    it "clean good ABI file" $
      checkFile "test/ABI/test.ab1" 465 428 "AGGGGT"

    it "clean another good ABI file" $
      checkFile "test/ABI/bad_at_the_end.ab1" 1116 955 "TTCCTT"

    it "totally clean bad ABI file" $ do
      Right dat <- readData "test/ABI/bad_quality.ab1"
      clean dat `shouldBe` Nothing
  where
    checkFile :: FilePath -> Int -> Int -> String -> IO ()
    checkFile path lengthBefore lengthAfter start = do
        Right dat <- readData path
        S.length dat `shouldBe` lengthBefore

        let Just cleaned = clean dat
        S.length cleaned `shouldBe` lengthAfter

        S.toList cleaned `shouldStartWith` start

readData :: FilePath -> IO (Either Text BasecalledSequence)
readData path = do
    bsl <- BSL.readFile path
    pure $ sequenceDecode bsl


goodSequence :: String
goodSequence = "AATTGGCAGTATTTAGTAATAACAAATAGGGGTTCCGCGCACATTTCCCCGAAAAGTGCCACCTGCGGCCGCTGTACACTAGTGATCGTACGGGCCCATGCATGCTAGCAAGCTTGTCGACATTACCCTGTTATCCCTATTCGCTACCTTAGGACCGTTATAGTTACGACCCATACACTAGTGATCGTACGGGCCCATGCATGCTAGCAAGCTTGTCGACATTACCCTGTTATCCCTATTCGCTACCTTAGGACCGTTATAGTTACGCTTGTCGACATTACCCTGTTATCCCTATTCGCTACCTTAGGACCGTTATAGTTACGACCCATAATACCCATAATAGCTGTTTGCCAATCTAGAGGTACCTCCGGAATGTCGCTTCCTCGCTCACTGACTCGCTGCGCTCGGTCGTTCGGCTGCGGCGAGCGGTATCAGCTCACTCAAAGGCGGTAATACGGTTATCAA"

goodQuality :: [Double]
goodQuality = [11.0,6.0,3.0,3.0,3.0,3.0,4.0,4.0,6.0,4.0,5.0,11.0,23.0,6.0,14.0,5.0,4.0,10.0,26.0,6.0,7.0,7.0,26.0,26.0,26.0,51.0,39.0,41.0,51.0,58.0,49.0,49.0,58.0,54.0,58.0,58.0,54.0,58.0,36.0,29.0,48.0,46.0,28.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,41.0,41.0,62.0,62.0,62.0,47.0,49.0,62.0,62.0,49.0,62.0,54.0,54.0,47.0,41.0,24.0,24.0,27.0,22.0,27.0,22.0,22.0,14.0,11.0,16.0,32.0,51.0,62.0,59.0,59.0,59.0,59.0,28.0,35.0,54.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,59.0,59.0,59.0,62.0,62.0,59.0,62.0,59.0,59.0,46.0,54.0,48.0,62.0,59.0,62.0,62.0,59.0,59.0,59.0,62.0,59.0,59.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,54.0,54.0,59.0,59.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,54.0,54.0,62.0,54.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,59.0,59.0,62.0,59.0,59.0,59.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,59.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,59.0,59.0,50.0,59.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,54.0,62.0,62.0,54.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,59.0,62.0,51.0,62.0,62.0,62.0,62.0,54.0,49.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,51.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,56.0,56.0,62.0,62.0,56.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,43.0,43.0,62.0,62.0,62.0,59.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,59.0,59.0,46.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,56.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,62.0,49.0,49.0,62.0,62.0,62.0,62.0,56.0,42.0,42.0,56.0,46.0,56.0,43.0,36.0,43.0,51.0,62.0,7.0,8.0,8.0,9.0,5.0]


