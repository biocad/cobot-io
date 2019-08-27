{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module SequenceSpec where

import           Bio.Sequence       (BareSequence, IsMarking, IsWeight (..),
                                     MarkedSequence, Sequence, WeightedSequence,
                                     addMarkings, bareSequence, createSequence,
                                     drop, getMarking, getRange, getWeight,
                                     markedSequence, mean, meanInRange, reverse,
                                     tail, take, toMarked, toWeighted,
                                     unsafeCreateSequence, unsafeMarkedSequence,
                                     unsafeWeightedSequence, weightedSequence)
import qualified Data.List.NonEmpty as NE (fromList)
import           Data.Text          (Text)
import           Prelude            hiding (drop, reverse, tail, take)
import           Test.Hspec

instance IsWeight Int where
  toDouble = fromIntegral

type TestWeightedSequence = WeightedSequence Int Char

weightedSequenceSpec :: Spec
weightedSequenceSpec =
  describe "Weighted sequence" $ do
    it "successful creation of weighted sequence" $ do
      let seqE = weightedSequence ['a', 'b', 'c', 'd'] [1, 2, 3, 4] :: Either Text TestWeightedSequence
      seqE `shouldBe` Right (unsafeWeightedSequence ['a', 'b', 'c', 'd'] [1, 2, 3, 4])

    it "unsuccessful creation of weighted sequence" $ do
      let seqErr  = Left "Bio.Sequence.Class: sequence and weights have different lengths."
      let seqErr1 = Left "Bio.Sequence.Class: weights are null for sequence."

      let seqE = weightedSequence ['a', 'b', 'c', 'd'] [1, 2, 4] :: Either Text TestWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = weightedSequence ['a', 'b', 'c', 'd'] [] :: Either Text TestWeightedSequence
      seqE `shouldBe` seqErr1

      let seqE = weightedSequence [] [1] :: Either Text TestWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = weightedSequence [] [] :: Either Text TestWeightedSequence
      seqE `shouldBe` seqErr1

newtype TestMarking = TestMarking Text
  deriving (Eq, Show, Ord)

instance IsMarking TestMarking

type TestMarkedSequence = MarkedSequence TestMarking Char

markedSequenceSpec :: Spec
markedSequenceSpec =
  describe "Marked sequence" $ do
    it "successful creation of marked sequence" $ do
      let seqE = markedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] :: Either Text TestMarkedSequence
      seqE `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))])

      let seqE = markedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "bca", (1, 5)), (TestMarking "a", (3, 5))] :: Either Text TestMarkedSequence
      seqE `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "bca", (1, 5)), (TestMarking "a", (3, 5))])

      let seqE = markedSequence [] [] :: Either Text TestMarkedSequence
      seqE `shouldBe` Right (unsafeMarkedSequence [] [])

      let seqE = markedSequence ['a', 'b', 'c', 'a', 'a'] [] :: Either Text TestMarkedSequence
      seqE `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [])
    it "unsuccessful creation of marked sequence" $ do
      let seqErr = Left "Bio.Sequence.Class: invalid 'Range' found in sequence's marking."

      let seqE = markedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (-1, 1))] :: Either Text TestMarkedSequence
      seqE `shouldBe` seqErr

      let seqE = markedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "bca", (0, 6)), (TestMarking "a", (3, 5))] :: Either Text TestMarkedSequence
      seqE `shouldBe` seqErr

      let seqE = markedSequence ['a'] [(TestMarking "a", (0, 0))] :: Either Text TestMarkedSequence
      seqE `shouldBe` seqErr

      let seqE = markedSequence [] [(TestMarking "k", (0, 1))] :: Either Text TestMarkedSequence
      seqE `shouldBe` seqErr

type TestMarkedAndWeightedSequence = Sequence TestMarking Int Char

markedAndWeightedSequenceSpec :: Spec
markedAndWeightedSequenceSpec =
  describe "Marked and weighted sequence" $ do
    it "successful creation of marked and weighted sequence" $ do
      let seqE = createSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` Right (unsafeCreateSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 5])

    it "unsuccessful creation of marked and weighted sequence" $ do
      let seqErr = Left "Bio.Sequence.Class: invalid 'Range' found in sequence's marking."

      let seqE = createSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (-1, 1))] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = createSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "bca", (0, 6)), (TestMarking "a", (3, 5))] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = createSequence ['a'] [(TestMarking "a", (0, 0))] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = createSequence [] [(TestMarking "k", (0, 1))] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = createSequence ['a', 'b', 'c', 'd'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2, 4] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr

      let seqE = createSequence ['a', 'b', 'c', 'a', 'a'] [] [1, 2.. 5] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` Right (unsafeCreateSequence ['a', 'b', 'c', 'a', 'a'] [] [1, 2.. 5])

      let seqErr1 = Left "Bio.Sequence.Class: sequence and weights have different lengths."

      let seqE = createSequence ['a', 'b', 'c', 'd'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 4))] [1, 2] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr1

      let seqErr2 = Left "Bio.Sequence.Class: weights are null for sequence."

      let seqE = createSequence ['a', 'b', 'c', 'd'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 4))] [] :: Either Text TestMarkedAndWeightedSequence
      seqE `shouldBe` seqErr2

functionsSpec :: Spec
functionsSpec =
  describe "Sequence functions" $ do
    -- common 'IsSequence' functions
    getRangeSpec
    reverseSpec
    dropSpec
    takeSpec
    tailSpec

    -- 'Marking functions
    getMarkingSpec
    toMarkedSpec
    addMarkingsSpec

    -- 'Weight' functions
    meanAndMeanInRangeSpec
    getWeightSpec
    toWeightedSpec

getRangeSpec :: Spec
getRangeSpec =
  describe "getRange" $ do
    let getRangeError = Left "Bio.Sequence.Functions.Sequence: invalid range in getRange."
    let s = unsafeCreateSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 5] :: TestMarkedAndWeightedSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; range: (0, 1)" $ do
      getRange s (0, 1) `shouldBe` Right ['a']
    it "sequence: ['a', 'b', 'c', 'a', 'a']; range: (2, 5)" $ do
      getRange s (2, 5) `shouldBe` Right ['c', 'a', 'a']
    it "sequence: ['a', 'b', 'c', 'a', 'a', 'd', 'e']; range: (5, 7)" $ do
      let s = unsafeCreateSequence ['a', 'b', 'c', 'a', 'a', 'd', 'e'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 7] :: TestMarkedAndWeightedSequence
      getRange s (5, 7) `shouldBe` Right ['d', 'e']
    it "sequence: ['a', 'b', 'c', 'a', 'a']; range: (0, 0)" $ do
      getRange s (0, 0) `shouldBe` getRangeError
    it "sequence: ['a', 'b', 'c', 'a', 'a']; range: (3, 8)" $ do
      getRange s (3, 8) `shouldBe` getRangeError

reverseSpec :: Spec
reverseSpec =
  describe "reverse" $ do
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; weights: [1, 2.. 5]" $ do
      let s = unsafeCreateSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 5] :: TestMarkedAndWeightedSequence
      reverse s `shouldBe` unsafeCreateSequence ['a', 'a', 'c', 'b', 'a'] [(TestMarking "a", (4, 5)), (TestMarking "a", (0, 2))] [5, 4.. 1]
    it "sequence: ['a', 'b']; markings: []; weights: [1, 2]" $ do
      let s = unsafeWeightedSequence ['a', 'b'] [1, 2] :: TestWeightedSequence
      reverse s `shouldBe` unsafeWeightedSequence ['b', 'a'] [2, 1]
    it "sequence: ['a', 'b', 'c', 'd', 'e']; markings: [(abc, (0, 3)), (abcd, (0, 4)), (de, (3, 5)), (abcde, (0, 5))]; weights: []" $ do
      let s = unsafeMarkedSequence ['a', 'b', 'c', 'd', 'e'] [(TestMarking "abc", (0, 3)), (TestMarking "abcd", (0, 4)), (TestMarking "de", (3, 5)), (TestMarking "abcde", (0, 5))] :: TestMarkedSequence
      reverse s `shouldBe` unsafeMarkedSequence ['e', 'd', 'c', 'b', 'a'] [(TestMarking "abc", (2, 5)), (TestMarking "abcd", (1, 5)), (TestMarking "de", (0, 2)), (TestMarking "abcde", (0, 5))]

dropSpec :: Spec
dropSpec =
  describe "drop" $ do
    let s  = unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5] :: TestWeightedSequence
    let sB = bareSequence ['a', 'b', 'c', 'a', 'a'] :: TestBareSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; drop: 2" $ do
      drop 2 s `shouldBe` unsafeWeightedSequence ['c', 'a', 'a'] [3.. 5]
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; drop: 4" $ do
      drop 4 s `shouldBe` unsafeWeightedSequence ['a'] [5]
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; drop: 4" $ do
      drop 0 s `shouldBe` s
    it "sequence: ['a', 'b', 'c', 'a', 'a']; drop: 2" $ do
      drop 2 sB `shouldBe` bareSequence ['c', 'a', 'a']

takeSpec :: Spec
takeSpec =
  describe "take" $ do
    let s = unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5] :: TestWeightedSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; take: 2" $ do
      take 2 s `shouldBe` unsafeWeightedSequence ['a', 'b'] [1.. 2]
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; take: 8" $ do
      take 8 s `shouldBe` s

tailSpec :: Spec
tailSpec =
  describe "tail" $ do
    let s = unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5] :: TestWeightedSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]" $ do
      tail s `shouldBe` unsafeWeightedSequence ['b', 'c', 'a', 'a'] [2.. 5]
    it "sequence: ['b', 'c', 'a', 'a']; weights: [2.. 5]" $ do
      tail (tail s) `shouldBe` unsafeWeightedSequence ['c', 'a', 'a'] [3.. 5]
    it "sequence: [c', 'a', 'a']; weights: [3.. 5]" $ do
      tail (tail (tail s)) `shouldBe` unsafeWeightedSequence ['a', 'a'] [4.. 5]
    it "sequence: [a', 'a']; weights: [4.. 5]" $ do
      tail (tail (tail (tail s))) `shouldBe` unsafeWeightedSequence ['a'] [5]

getMarkingSpec :: Spec
getMarkingSpec =
  describe "getMarking" $ do
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; get: a" $ do
      let s = unsafeCreateSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] [1, 2.. 5] :: TestMarkedAndWeightedSequence
      getMarking s (TestMarking "a") `shouldBe` Right (NE.fromList [['a'], ['a', 'a']])
    it "sequence: ['a', 'b', 'c', 'd', 'e']; markings: [(abc, (0, 3)), (abcd, (0, 4)), (de, (3, 5)), (abcde, (0, 5))]; get: abcde" $ do
      let s = unsafeMarkedSequence ['a', 'b', 'c', 'd', 'e'] [(TestMarking "abc", (0, 3)), (TestMarking "abcd", (0, 4)), (TestMarking "de", (3, 5)), (TestMarking "abcde", (0, 5))] :: TestMarkedSequence
      getMarking s (TestMarking "abcde") `shouldBe` Right (NE.fromList [['a', 'b', 'c', 'd', 'e']])

type TestBareSequence = BareSequence Char

toMarkedSpec :: Spec
toMarkedSpec =
  describe "toMarked" $ do
    let s = bareSequence ['a', 'b', 'c', 'a', 'a'] :: TestBareSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]" $ do
      (toMarked s [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] :: Either Text TestMarkedSequence) `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))])
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 6)), (a, (3, 5))]" $ do
      let rangesError = Left "Bio.Sequence.Class: invalid 'Range' found in sequence's marking."
      (toMarked s [(TestMarking "a", (0, 6)), (TestMarking "a", (3, 5))] :: Either Text TestMarkedSequence) `shouldBe` rangesError

addMarkingsSpec :: Spec
addMarkingsSpec =
  describe "addMarkings" $ do
    let s = unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5))] :: TestMarkedSequence
    let rangesError = Left "Bio.Sequence.Functions.Marking: can't add markings to Sequence, because some of them are out of range."

    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; add: [(b, (1, 2))]" $ do
      addMarkings s [(TestMarking "b", (1, 2))] `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5)), (TestMarking "b", (1, 2))])
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; add: []" $ do
      addMarkings s [] `shouldBe` Right s
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; add: [(b, (1, 2)), (c, (2, 3)), (abcaa, (0, 5))]" $ do
      addMarkings s [(TestMarking "b", (1, 2)), (TestMarking "c", (2, 3)), (TestMarking "abcaa", (0, 5))] `shouldBe` Right (unsafeMarkedSequence ['a', 'b', 'c', 'a', 'a'] [(TestMarking "a", (0, 1)), (TestMarking "a", (3, 5)), (TestMarking "b", (1, 2)), (TestMarking "c", (2, 3)), (TestMarking "abcaa", (0, 5))])
    it "sequence: ['a', 'b', 'c', 'a', 'a']; markings: [(a, (0, 1)), (a, (3, 5))]; add: [(b, (1, 2)), (c, (2, 3)), (abcaa, (0, 6))]" $ do
      addMarkings s [(TestMarking "b", (1, 2)), (TestMarking "c", (2, 3)), (TestMarking "abcaa", (0, 6))] `shouldBe` rangesError

meanAndMeanInRangeSpec :: Spec
meanAndMeanInRangeSpec =
  describe "mean and meanInRange" $ do
    let s = unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5] :: TestWeightedSequence

    it "mean for sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]" $ do
      mean s `shouldBe` 3
    it "meanInRange for sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; range: (2, 4)" $ do
      meanInRange s (2, 4) `shouldBe` 4
    it "meanInRange for sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; range: (0, 5)" $ do
      meanInRange s (0, 5) `shouldBe` 3
    it "meanInRange for sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; range: (0, 0)" $ do
      meanInRange s (0, 0) `shouldBe` 1

getWeightSpec :: Spec
getWeightSpec =
  describe "getWeight" $ do
    let s = unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5] :: TestWeightedSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; ind: 2" $ do
      getWeight s 2 `shouldBe` Right 3
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; ind: 4" $ do
      getWeight s 4 `shouldBe` Right 5
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]; ind: 7" $ do
      getWeight s 7 `shouldBe` Left "Bio.Sequence.Functions.Weight: index out of range."

toWeightedSpec :: Spec
toWeightedSpec =
  describe "toWeighted" $ do
    let s = bareSequence ['a', 'b', 'c', 'a', 'a'] :: TestBareSequence

    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 5]" $ do
      (toWeighted s [1.. 5] :: Either Text TestWeightedSequence) `shouldBe` Right (unsafeWeightedSequence ['a', 'b', 'c', 'a', 'a'] [1.. 5])
    it "sequence: ['a', 'b', 'c', 'a', 'a']; weights: [1.. 3]" $ do
      let rangesError = Left "Bio.Sequence.Class: sequence and weights have different lengths."
      (toWeighted s [1.. 3] :: Either Text TestWeightedSequence) `shouldBe` rangesError
