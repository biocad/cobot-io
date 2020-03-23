{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StructureSpec where

import           Bio.MAE                 (fromFile)
import           Bio.Structure           (Atom (..), Bond (..), Chain (..),
                                          GlobalID (..), LocalID (..),
                                          Model (..), Residue (..),
                                          StructureModels (..))
import           Bio.Structure.Functions (filterAtomsOfModel)
import           Data.Set                (Set)
import qualified Data.Set                as S (fromList)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V (all, fromList, toList, (!))
import           Linear.V3               (V3 (..))
import           Test.Hspec

structureSpec :: Spec
structureSpec = describe "Structure spec." $ do
    m <- runIO $ V.toList . modelsOf <$> fromFile "test/MAE/small.mae" >>= \[x] -> pure x

    it "atoms filtering works correctly. only CAs" $ do
        let Model{..} = filterAtomsOfModel ((== "CA") . atomName) m

        V.all (V.all ((== V.fromList ["CA"]) . fmap atomName . resAtoms) . chainResidues) resChains `shouldBe` True
