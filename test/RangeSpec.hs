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
    rangeExtendTests

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
        flip overlap (Point 42) (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) `shouldBe` False
    it "Join-Point, should overlap" $ 
        flip overlap (Point 42) (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) `shouldBe` True

    it "Point-Complement, should not overlap" $ 
        overlap (Point 42) (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) `shouldBe` False
    it "Point-Complement, should overlap" $ 
        overlap (Point 42) (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) `shouldBe` True
    it "Complement-Point, should not overlap" $ 
        flip overlap (Point 42) (Join [Point 24, preciseSpan (29, 41), Between 42 43, preciseSpan (77, 99)]) `shouldBe` False
    it "Complement-Point, should overlap" $ 
        flip overlap (Point 42) (Join [Point 10, Point 20, preciseSpan (30, 50), Between 88 89]) `shouldBe` True

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

    it "Span-Join, should not overlap" $
        -- ---xxxxxxxx--------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (preciseSpan (5, 10)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` False
    it "Span-Join, should overlap" $
        -- ----xxxxxxxx-------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (preciseSpan (6, 11)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Span-Join, should overlap" $
        -- ----------xxxxxxxx-------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (preciseSpan (9, 18)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Span-Join, should overlap" $
        -- ------------------xxxxxxxx-----------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (preciseSpan (18, 32)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Span-Join, should overlap" $
        -- -----------------------------xxxxxxxx
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (preciseSpan (40, 50)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Span, should not overlap" $
        -- ---xxxxxxxx--------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (preciseSpan (5, 10)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` False
    it "Join-Span, should overlap" $
        -- ----xxxxxxxx-------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (preciseSpan (6, 11)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Span, should overlap" $
        -- ----------xxxxxxxx-------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (preciseSpan (9, 18)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Span, should overlap" $
        -- ------------------xxxxxxxx-----------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (preciseSpan (18, 32)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Span, should overlap" $
        -- -----------------------------xxxxxxxx
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (preciseSpan (40, 50)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True

    it "Span-Complement, should not overlap" $
        --  -----xxxxxxxxxxxxx---------------------------
        --  --------------------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (30, 40)) `shouldBe` False
    it "Span-Complement, should not overlap" $
        --  -----xxxxxxxxxxxxx-------------------
        --  ------------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (21, 40)) `shouldBe` False
    it "Span-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx------------------
        --  -----------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (20, 40)) `shouldBe` True 
    it "Span-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx---------------
        --  --------------xxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (17, 40)) `shouldBe` True 
    it "Span-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx----------------
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (5, 40)) `shouldBe` True 
    it "Span-Complement, should overlap" $
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        --  -----xxxxxxxxxxxxx----------------
        overlap (preciseSpan (5, 40)) (Complement $ preciseSpan (10, 20)) `shouldBe` True 
    it "Complement-Span, should not overlap" $
        --  -----xxxxxxxxxxxxx---------------------------
        --  --------------------------xxxxxxxxxxxxx------
        flip overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (30, 40)) `shouldBe` False
    it "Complement-Span, should not overlap" $
        --  -----xxxxxxxxxxxxx-------------------
        --  ------------------xxxxxxxxxxxxx------
        flip overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (21, 40)) `shouldBe` False
    it "Complement-Span, should overlap" $
        --  -----xxxxxxxxxxxxx------------------
        --  -----------------xxxxxxxxxxxxx------
        flip overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (20, 40)) `shouldBe` True 
    it "Complement-Span, should overlap" $
        --  -----xxxxxxxxxxxxx---------------
        --  --------------xxxxxxxxxxxxx------
        flip overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (17, 40)) `shouldBe` True 
    it "Complement-Span, should overlap" $
        --  -----xxxxxxxxxxxxx----------------
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        flip overlap (preciseSpan (10, 20)) (Complement $ preciseSpan (5, 40)) `shouldBe` True 
    it "Complement-Span, should overlap" $
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        --  -----xxxxxxxxxxxxx----------------
        flip overlap (preciseSpan (5, 40)) (Complement $ preciseSpan (10, 20)) `shouldBe` True 

    it "Join-Join, should not overlap" $
        -- ---xxx--|--xxxxx----x----xxxx------
        -- x|-----x---------xx---xx-------xxxx
        overlap (Join [preciseSpan (5, 10), Between 15 16, preciseSpan (30, 40), Point 50, preciseSpan (60, 70)]) (Join [Point 1, Between 1 2, Point 15, preciseSpan (42, 46), preciseSpan (52, 55), preciseSpan (80, 100)]) `shouldBe` False
    it "Join-Join, should overlap" $
        -- ---xxx--|--xxxxx----x----xxxx---
        -- x|-----x---------xx---xx----xxxx
        overlap (Join [preciseSpan (5, 10), Between 15 16, preciseSpan (30, 40), Point 50, preciseSpan (60, 70)]) (Join [Point 1, Between 1 2, Point 15, preciseSpan (42, 46), preciseSpan (52, 55), preciseSpan (70, 100)]) `shouldBe` True 
    it "Join-Join, should overlap" $
        -- ---xxx--|--xxxxx--x------xxxx------
        -- x|-----x---------xx---xx-------xxxx
        overlap (Join [preciseSpan (5, 10), Between 15 16, preciseSpan (30, 40), Point 46, preciseSpan (60, 70)]) (Join [Point 1, Between 1 2, Point 15, preciseSpan (42, 46), preciseSpan (52, 55), preciseSpan (80, 100)]) `shouldBe` True 

    it "Join-Complement, should not overlap" $
        -- ---xxxxxxxx--------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (Complement $ preciseSpan (5, 10)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` False
    it "Join-Complement, should overlap" $
        -- ----xxxxxxxx-------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (Complement $ preciseSpan (6, 11)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Complement, should overlap" $
        -- ----------xxxxxxxx-------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (Complement $ preciseSpan (9, 18)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Complement, should overlap" $
        -- ------------------xxxxxxxx-----------
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (Complement $ preciseSpan (18, 32)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Join-Complement, should overlap" $
        -- -----------------------------xxxxxxxx
        -- -x---------|--xxxxx---xxxxxxxx-------
        flip overlap (Complement $ preciseSpan (40, 50)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Complement-Join, should not overlap" $
        -- ---xxxxxxxx--------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (Complement $ preciseSpan (5, 10)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` False
    it "Complement-Join, should overlap" $
        -- ----xxxxxxxx-------------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (Complement $ preciseSpan (6, 11)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Complement-Join, should overlap" $
        -- ----------xxxxxxxx-------------------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (Complement $ preciseSpan (9, 18)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Complement-Join, should overlap" $
        -- ------------------xxxxxxxx-----------
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (Complement $ preciseSpan (18, 32)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True
    it "Complement-Join, should overlap" $
        -- -----------------------------xxxxxxxx
        -- -x---------|--xxxxx---xxxxxxxx-------
        overlap (Complement $ preciseSpan (40, 50)) (Join [Point 2, Between 10 11, preciseSpan (15, 20), preciseSpan (30, 40)]) `shouldBe` True

    it "Complement-Complement, should not overlap" $
        --  -----xxxxxxxxxxxxx---------------------------
        --  --------------------------xxxxxxxxxxxxx------
        overlap (Complement $ preciseSpan (10, 20)) (Complement $ preciseSpan (30, 40)) `shouldBe` False
    it "Complement-Complement, should not overlap" $
        --  -----xxxxxxxxxxxxx-------------------
        --  ------------------xxxxxxxxxxxxx------
        overlap (Complement $ preciseSpan (10, 20)) (Complement $ preciseSpan (21, 40)) `shouldBe` False
    it "Complement-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx------------------
        --  -----------------xxxxxxxxxxxxx------
        overlap (Complement $ preciseSpan (10, 20)) (Complement $ preciseSpan (20, 40)) `shouldBe` True 
    it "Complement-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx---------------
        --  --------------xxxxxxxxxxxxx------
        overlap (Complement $ preciseSpan (10, 20)) (Complement $ preciseSpan (17, 40)) `shouldBe` True 
    it "Complement-Complement, should overlap" $
        --  -----xxxxxxxxxxxxx----------------
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        overlap (Complement $ preciseSpan (10, 20)) (Complement $ preciseSpan (5, 40)) `shouldBe` True 
    it "Complement-Complement, should overlap" $
        --  ---xxxxxxxxxxxxxxxxxxxxxxxxx------
        --  -----xxxxxxxxxxxxx----------------
        overlap (Complement $ preciseSpan (5, 40)) (Complement $ preciseSpan (10, 20)) `shouldBe` True 


rangeShiftTests :: Spec
rangeShiftTests = describe "Range shift tests" $ do
    it "Shifts a point" $ shiftRange 22 (Point 20) `shouldBe` Point 42
    it "Shifts a Between feature" $ shiftRange 22 (Between 20 21) `shouldBe` Between 42 43
    it "Shifts a span" $ shiftRange 22 (preciseSpan (20, 21)) `shouldBe` preciseSpan (42, 43)
    it "Shifts a Join feature" $ shiftRange 22 (Join [Point 10, Between 11 12, preciseSpan (13, 14)]) `shouldBe` Join [Point 32, Between 33 34, preciseSpan (35, 36)]
    it "Shifts a Complement feature" $ shiftRange 22 (Complement $ Point 20) `shouldBe` Complement (Point 42)

rangeMapTests :: Spec
rangeMapTests = describe "Range map tests" $ do
    it "Maps a point" $ mapRange ((-) 1) (Point 20) `shouldBe` Point (-19)
    it "Maps a Between feature" $ mapRange ((-) 1) (Between 20 21) `shouldBe` Between (-19) (-20) 
    it "Maps a span" $ mapRange ((-) 1) (preciseSpan (20, 21)) `shouldBe` preciseSpan (-19, -20)
    it "Maps a Join feature" $ mapRange ((-) 1) (Join [Point 10, Between 11 12, preciseSpan (13, 14)]) `shouldBe` Join [Point (-9), Between (-10) (-11), preciseSpan (-12, -13)]
    it "Maps a Complement feature" $ mapRange ((-) 1) (Complement $ Point 20) `shouldBe` Complement (Point (-19))

rangeExtendTests :: Spec
rangeExtendTests = do
    describe "Range extend tests" $ do
        describe "Extend left" $ do
            it "Extends a point" $ extendLeft 10 (Point 10) `shouldBe` preciseSpan (0, 10)
            it "Extends a Between feature" $ extendLeft 10 (Between 10 11) `shouldBe` Between 10 11
            it "Extends a span" $ extendLeft 10 (preciseSpan (10, 20)) `shouldBe` preciseSpan (0, 20)
            it "Extends a Join feature" $ extendLeft 10 (Join [Point 10, Between 11 12, preciseSpan (13, 14)]) `shouldBe` Join [preciseSpan (0, 10), Between 11 12, preciseSpan (3, 14)]
            it "Extends a Complement feature" $ extendLeft 10 (Complement $ Point 10) `shouldBe` (Complement $ preciseSpan (0, 10))
        describe "Extend right" $ do
            it "Extends a point" $ extendRight 10 (Point 10) `shouldBe` preciseSpan (10, 20)
            it "Extends a Between feature" $ extendRight 10 (Between 10 11) `shouldBe` Between 10 11
            it "Extends a span" $ extendRight 10 (preciseSpan (10, 20)) `shouldBe` preciseSpan (10, 30)
            it "Extends a Join feature" $ extendRight 10 (Join [Point 10, Between 11 12, preciseSpan (13, 14)]) `shouldBe` Join [preciseSpan (10, 20), Between 11 12, preciseSpan (13, 24)]
            it "Extends a Complement feature" $ extendRight 10 (Complement $ Point 10) `shouldBe` (Complement $ preciseSpan (10, 20))
