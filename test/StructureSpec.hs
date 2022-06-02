{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-incomplete-uni-patterns #-}

module StructureSpec where

import           Bio.MAE                 (fromFile)
import           Bio.Structure           (Atom (..), Bond (..), Chain (..),
                                          LocalID (..), Model (..),
                                          Residue (..), StructureModels (..))
import           Bio.Structure.Functions (filterAtomsOfModel)
import           Control.Monad           (join)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M (fromList, (!))
import           Data.Maybe              (fromJust, isJust)
import           Data.Set                (Set)
import qualified Data.Set                as S (fromList, member)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V (all, filter, find, fromList,
                                               toList, zip)
import           Test.Hspec

structureSpec :: Spec
structureSpec = describe "Structure spec." $ do
    let mIO = fromFile "test/MAE/Capri.mae" >>= (\[x] -> pure x) . V.toList . modelsOf
    beforeAll mIO $ do
        it "atoms filtering works correctly. only N, CA, C"  $ \m -> checkFiltering m $ (`elem` ["N", "CA", "C"]) . atomName
        it "atoms filtering works correctly. only CA"        $ \m -> checkFiltering m $ (== "CA") . atomName
        it "atoms filtering works correctly. no atoms"       $ \m -> checkFiltering m $ const False
        it "atoms filtering works correctly. all atoms"      $ \m -> checkFiltering m $ const True
  where
    checkFiltering :: Model -> (Atom -> Bool) -> Expectation
    checkFiltering m p = do
        checkAtoms
        checkGlobalBonds
        checkLocalBonds
      where
        m' = filterAtomsOfModel p m

        checkAtoms :: Expectation
        checkAtoms = V.all (V.all (V.all p . resAtoms) . chainResidues) (modelChains m') `shouldBe` True

        checkGlobalBonds :: Expectation
        checkGlobalBonds = all (\(Bond l r _) -> l `S.member` inds && r `S.member` inds) (modelBonds m') `shouldBe` True
          where
            filteredAtoms = join $ fmap (join . fmap (V.filter p . resAtoms) . chainResidues) $ modelChains m
            inds          = vecToSet $ fmap atomId filteredAtoms

        checkLocalBonds :: Expectation
        checkLocalBonds = all checkResiduePair pairsOfResidues `shouldBe` True
          where
            pairsOfResidues = zip (V.toList (join $ chainResidues <$> modelChains m))
                                  (V.toList (join $ chainResidues <$> modelChains m'))

            checkResiduePair :: (Residue, Residue) -> Bool
            checkResiduePair (r, r') = vecToSet mappedBonds == vecToSet (resBonds r')
              where
                atInds  = atomsWithIndices r
                atInds' = atomsWithIndices r'

                mappedBonds = fromJust <$> V.filter isJust (mapBond <$> resBonds r)

                mapBond :: Bond LocalID -> Maybe (Bond LocalID)
                mapBond (Bond l k t) = Bond <$> localMapping M.! l
                                            <*> localMapping M.! k
                                            <*> pure t

                localMapping :: Map LocalID (Maybe LocalID)
                localMapping = M.fromList $ V.toList forMap
                  where
                    forMap = fmap (\(a, i) -> (i, snd <$> V.find ((== atomId a) . atomId . fst) atInds')) atInds

                atomsWithIndices :: Residue -> Vector (Atom, LocalID)
                atomsWithIndices Residue{..} = V.zip resAtoms $ fmap LocalID $ V.fromList [0 ..]

        vecToSet :: Ord a => Vector a -> Set a
        vecToSet = S.fromList . V.toList

