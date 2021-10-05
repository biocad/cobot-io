{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module RangeSpec where

import Bio.Sequence (Range (..), extendLeft, extendRight, mapRange, overlap, preciseSpan,
                     shiftRange)
import Test.Hspec


rangeSpec :: Spec
rangeSpec = do
    rangeOverlapTests
    rangeShiftTests
    rangeMapTests
    rangeExtendTextx

rangeOverlapTests :: Spec
rangeOverlapTests = describe "Range overlap tests" $ do
    it "Point-Point, should not overlap" $ 
        overlap (Point 42) (Point 24) `shouldBe` False
    it "Point-Point, should overlap" $ 
        overlap (Point 42) (Point 42) `shouldBe` True

    it "Point-Span, should not overlap" $ 
        overlap (Point 42) (preciseSpan (14, 24)) `shouldBe` False
    it "Point-Span, should overlap" $ 
        overlap (Point 42) (preciseSpan (14, 42)) `shouldBe` True
    it "Point-Span, should overlap" $ 
        overlap (Point 42) (preciseSpan (14, 43)) `shouldBe` True
    it "Point-Span, should overlap" $ 
        overlap (Point 42) (preciseSpan (42, 50)) `shouldBe` True
    it "Span-Point, should not overlap" $ 
        overlap (preciseSpan (14, 24)) (Point 42) `shouldBe` False
    it "Span-Point, should overlap" $ 
        overlap (preciseSpan (14, 42)) (Point 42) `shouldBe` True
    it "Span-Point, should overlap" $ 
        overlap (preciseSpan (14, 43)) (Point 42) `shouldBe` True
    it "Span-Point, should overlap" $ 
        overlap (preciseSpan (42, 50)) (Point 42) `shouldBe` True

    it "Point-Between, should not overlap" $ 
        overlap (Point 42) (Between 42 43) `shouldBe` False
    it "Between-Point, should not overlap" $ 
        overlap (Between 23 24) (Point 24) `shouldBe` False

    it "Point-Join, should not overlap" $ 
        overlap (Point 42) (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) `shouldBe` False
    it "Point-join, should overlap" $ 
        overlap (Point 42) (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) `shouldBe` True
    it "Join-Point, should not overlap" $ 
        overlap (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) (Point 42) `shouldBe` False
    it "Join-Point, should overlap" $ 
        overlap (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) (Point 42) `shouldBe` True

    it "Point-Complement, should not overlap" $ 
        overlap (Point 42) (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) `shouldBe` False
    it "Point-Complement, should overlap" $ 
        overlap (Point 42) (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) `shouldBe` True
    it "Complement-Point, should not overlap" $ 
        overlap (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) (Point 42) `shouldBe` False
    it "Complement-Point, should overlap" $ 
        overlap (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) (Point 42) `shouldBe` True

    it "Span-Span, should not overlap" $
        --  -----xxxxxxxxxxxxx---------------------------
        --  --------------------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (preciseSpan (30, 40)) `shouldBe` False
    it "Span-Span, should not overlap" $
        --  -----xxxxxxxxxxxxx-------------------
        --  ------------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (preciseSpan (21, 40)) `shouldBe` False
    it "Span-Span, should overlap" $
        --  -----xxxxxxxxxxxxx------------------
        --  -----------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (preciseSpan (20, 40)) `shouldBe` True 
    it "Span-Span, should overlap" $
        --  -----xxxxxxxxxxxxx---------------
        --  --------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (preciseSpan (17, 40)) `shouldBe` True 
    it "Span-Span, should overlap" $
        --  -----xxxxxxxxxxxxx----------------
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (preciseSpan (5, 40)) `shouldBe` True 
    it "Span-Span, should overlap" $
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        --  -----xxxxxxxxxxxxx----------------
        overlap (preciseSpan (5, 40)) (preciseSpan (10, 20)) `shouldBe` True 

rangeShiftTests :: Spec
rangeShiftTests = describe "Range shift tests" $ fst (pure (), (extendLeft, (extendRight, (mapRange, shiftRange)))) 

rangeMapTests :: Spec
rangeMapTests = describe "Range map tests" $ pure ()

rangeExtendTextx :: Spec
rangeExtendTextx = describe "Range extend tests" $ pure ()
