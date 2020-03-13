{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MAESpec where

import           Bio.MAE       (fromFile)
import           Bio.Structure (Atom (..), Bond (..), Chain (..), GlobalID (..),
                                LocalID (..), Model (..), Residue (..),
                                StructureModels (..))
import           Data.Set      (Set)
import qualified Data.Set      as S (fromList)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V (fromList, toList, (!))
import           Linear.V3     (V3 (..))
import           Test.Hspec

maeSpec :: Spec
maeSpec = describe "Mae spec." $ do
    Model{..} <- runIO $ V.toList . modelsOf <$> fromFile "test/MAE/small.mae" >>= \[x] -> pure x

    let firstChainResidues = (chainResidues $ modelChains V.! 0)
    let secondChainResidues = (chainResidues $ modelChains V.! 1)

    it "two chains" $ length modelChains `shouldBe` 2
    
    it "residue numbers" $ do
        fmap resName firstChainResidues `shouldBe` V.fromList ["ACE", "ASP", "ILE", "LYS"]
        fmap resName secondChainResidues `shouldBe` V.fromList ["GLU", "LEU", "VAL", "ARG", "PRO", "GLY", "ALA", "LEU", "VAL"]
    
    it "residue names" $ do
        fmap resNumber firstChainResidues `shouldBe` V.fromList [0, 1, 2, 3]
        fmap resNumber secondChainResidues `shouldBe` V.fromList [10, 11, 12, 13, 13, 15, 16, 17, 18] -- 13 is doubled because the second 13 has 'A' insertion code

    it "residue insertion codes" $ do
        fmap resInsertionCode firstChainResidues `shouldBe` V.fromList [' ', ' ', ' ', ' ']
        fmap resInsertionCode secondChainResidues `shouldBe` V.fromList [' ', ' ', ' ', ' ', 'A', ' ', ' ', ' ', ' ']

    it "atoms count" $ do
        sum (fmap (length . resAtoms) firstChainResidues) `shouldBe` 62
        sum (fmap (length . resAtoms) secondChainResidues) `shouldBe` 140

    let allBonds = [ (2, 15, 1), (31, 44, 1), (15, 60, 1), (52, 27, 1), (57, 30, 1), (56, 8, 1), (38, 9, 1), (16, 36, 1), (31, 35, 1), (42, 22, 1), (38, 10, 1), (46, 40, 1), (36, 8, 1)
                   , (46, 7, 1), (35, 13, 1), (60, 31, 1), (40, 32, 1), (46, 45, 1), (29, 1, 1), (9, 53, 1), (22, 33, 1), (41, 18, 1), (45, 31, 1), (13, 53, 1), (40, 19, 1), (55, 20, 1)
                   , (16, 22, 1), (51, 36, 1), (46, 56, 1), (8, 17, 1), (56, 40, 1), (50, 6, 1), (57, 16, 1), (57, 34, 1), (48, 47, 1), (28, 48, 1), (57, 30, 1), (39, 40, 1), (11, 6, 1)
                   , (35, 55, 1), (47, 30, 1), (4, 35, 1), (60, 35, 1), (39, 56, 1), (44, 24, 1), (29, 55, 1), (29, 41, 1), (6, 55, 1), (52, 51, 1), (32, 21, 1), (55, 55, 1), (43, 55, 1)
                   , (30, 44, 1), (54, 47, 1), (13, 50, 1), (14, 56, 1), (44, 54, 1), (58, 21, 1), (27, 58, 1), (84, 122, 2), (122, 122, 2), (109, 79, 2), (103, 121, 2), (102, 139, 2)
                   , (93, 93, 2), (68, 103, 2), (110, 77, 2), (109, 63, 2), (86, 83, 2), (123, 137, 2), (110, 131, 2), (122, 85, 2), (75, 110, 2), (131, 138, 2), (88, 134, 2), (117, 81, 2)
                   , (107, 74, 2), (67, 61, 2), (97, 134, 2), (94, 131, 2), (65, 95, 2), (124, 100, 2), (120, 106, 2), (71, 111, 2), (95, 129, 2), (104, 116, 2), (61, 95, 2), (88, 132, 2)
                   , (80, 97, 2), (68, 121, 2), (138, 87, 2), (84, 134, 2), (86, 139, 2), (84, 76, 2), (83, 85, 2), (76, 61, 2), (116, 98, 2), (64, 82, 2), (106, 93, 2), (96, 102, 2), (98, 122, 2)
                   , (74, 82, 2), (123, 130, 2), (114, 127, 2), (102, 122, 2), (90, 126, 2), (118, 92, 2), (88, 71, 2), (120, 132, 2), (135, 71, 2), (125, 136, 2), (71, 67, 2), (128, 110, 2)
                   , (67, 95, 2), (83, 72, 2), (60, 70, 2), (99, 135, 2), (87, 87, 2), (63, 124, 2), (64, 110, 2), (127, 81, 2), (90, 106, 2), (93, 92, 2), (62, 91, 2), (67, 119, 2), (95, 70, 2)
                   , (134, 115, 2), (113, 115, 2), (93, 112, 2), (102, 106, 2), (136, 105, 2), (82, 126, 2), (109, 103, 2), (66, 119, 2), (84, 60, 2), (79, 67, 2), (63, 96, 2), (74, 134, 2), (112, 91, 2)
                   , (105, 68, 2), (125, 80, 2), (136, 75, 2), (62, 117, 2), (66, 124, 2), (105, 120, 2), (141, 63, 2), (63, 90, 2), (139, 60, 2), (77, 73, 2), (81, 118, 2), (81, 72, 2), (123, 126, 2)
                   , (128, 136, 2), (83, 91, 2), (86, 124, 2), (140, 108, 2), (126, 98, 2), (67, 92, 2), (131, 69, 2), (69, 94, 2), (137, 66, 2), (99, 87, 2), (134, 134, 2), (134, 72, 2), (91, 138, 2)
                   , (89, 97, 2), (109, 128, 2), (123, 100, 2), (61, 64, 2), (68, 114, 2), (119, 115, 2), (122, 106, 2), (131, 84, 2), (93, 94, 2), (124, 118, 2), (82, 119, 2), (107, 95, 2), (112, 64, 2)
                   , (71, 71, 2), (89, 62, 2), (89, 61, 2), (120, 80, 2), (85, 61, 2), (97, 95, 2), (94, 98, 2), (98, 139, 2), (115, 133, 2), (115, 64, 2), (136, 66, 2), (131, 62, 2), (83, 102, 2)
                   , (121, 60, 2), (117, 110, 2), (134, 138, 2), (119, 89, 2), (137, 128, 2), (112, 118, 2), (105, 116, 2), (95, 69, 2)
                   ]

    it "global bonds" $ do
        modelBonds `shouldBe` toBond GlobalID <$> V.fromList allBonds

    let residue1 = firstChainResidues V.! 3
    let residue2 = secondChainResidues V.! 0

    it "atoms in residues" $ do
        let atoms1 = resAtoms residue1
        let atoms2 = resAtoms residue2

        fmap atomId atoms1 `shouldBe` GlobalID <$> V.fromList ([37 .. 58] <> [199 .. 201])
        fmap atomId atoms2 `shouldBe` GlobalID <$> V.fromList [59 .. 73]

        fmap atomName atoms1 `shouldBe` V.fromList [ "N", "CA", "C", "O", "CB", "CG", "CD", "CE", "NZ", "H"
                                                   , "HA", "HB3", "HB2", "HG3", "HG2", "HD3", "HD2", "HE3"
                                                   , "HE2", "HZ1", "HZ2", "HZ3", "2H", "CG2", "HA"
                                                   ]
        fmap atomName atoms2 `shouldBe` V.fromList [ "N", "CA", "C", "O", "CB", "CG", "CD", "OE1", "OE2", "H"
                                                   , "HA", "HB3", "HB2", "HG3", "HG2"
                                                   ]

        fmap atomCoords atoms1 `shouldBe` V.fromList [ V3 (-3.444000) 7.750000 48.589000, V3 (-2.336000) 8.349000 47.850000, V3 (-1.127000) 7.408000 47.751000, V3 (-0.731000) 6.818000 48.755000, V3 (-1.974000) 9.706000 48.503000
                                                     , V3 (-0.665000) 10.349000 47.988000, V3 (-0.475000) 11.818000 48.390000, V3 (-1.315000) 12.784000 47.541000, V3 (-1.050000) 14.186000 47.903000, V3 (-3.565000) 8.072000 49.540000
                                                     , V3 (-2.692000) 8.558000 46.839000, V3 (-1.882000) 9.581000 49.583000, V3 (-2.810000) 10.388000 48.353000, V3 (-0.610000) 10.265000 46.901000, V3 0.184000 9.784000 48.375000
                                                     , V3 0.582000 12.073000 48.302000, V3 (-0.729000) 11.939000 49.444000, V3 (-2.380000) 12.585000 47.665000, V3 (-1.084000) 12.653000 46.483000, V3 (-1.609000) 14.795000 47.322000
                                                     , V3 (-1.289000) 14.335000 48.873000, V3 (-0.072000) 14.393000 47.762000, V3 (-9.600000) 7.518000 44.746000, V3 (-6.594000) 4.327000 47.986000, V3 (-5.134000) 6.683000 50.072000
                                                     ]
        fmap atomCoords atoms2 `shouldBe` V.fromList [ V3 (-8.382000) 11.633000 16.946000, V3 (-9.715000) 12.157000 17.191000, V3 (-9.590000) 13.665000 17.450000, V3 (-9.030000) 14.054000 18.475000, V3 (-10.323000) 11.388000 18.396000
                                                     , V3 (-11.833000) 11.576000 18.604000, V3 (-12.658000) 10.923000 17.495000, V3 (-12.525000) 9.689000 17.338000, V3 (-13.405000) 11.669000 16.827000, V3 (-7.615000) 12.187000 17.300000
                                                     , V3 (-10.322000) 11.996000 16.297000, V3 (-9.798000) 11.676000 19.305000, V3 (-10.128000) 10.318000 18.313000, V3 (-12.084000) 12.633000 18.684000, V3 (-12.123000) 11.111000 19.544000
                                                     ]

    it "bonds in residues" $ do
        toSet (resBonds residue1) `shouldBe` toSet (V.fromList $ toBond LocalID . (\(x, y, o) -> (x - 37, y - 37, o)) <$> filter (\(x, y, _) -> x - 1 `elem` [37 .. 58] && y - 1 `elem` [37 .. 58]) allBonds)
        toSet (resBonds residue2) `shouldBe` toSet (V.fromList $ toBond LocalID . (\(x, y, o) -> (x - 59, y - 59, o)) <$> filter (\(x, y, _) -> x - 1 `elem` [59 .. 73] && y - 1 `elem` [59 .. 73]) allBonds)
  where
    toBond :: (Int -> b) -> (Int, Int, Int) -> Bond b
    toBond f (x, y, o) = Bond (f $ x - 1) (f $ y - 1) o

    toSet :: Ord a => Vector a -> Set a
    toSet = S.fromList . V.toList

instance Ord (Bond LocalID) where
    (Bond (LocalID x) (LocalID y) _) <= (Bond (LocalID x') (LocalID y') _) | x == x'   = y <= y'
                                                                           | otherwise = x <= x'

instance Ord (Bond GlobalID) where
    (Bond (GlobalID x) (GlobalID y) _) <= (Bond (GlobalID x') (GlobalID y') _) | x == x'   = y <= y'
                                                                               | otherwise = x <= x'
