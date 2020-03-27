{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.PDB
  (
  ) where

import qualified Bio.PDB.Type  as PDB
import           Bio.Structure

import           Control.Arrow ((&&&))
import           Data.Coerce   (coerce)
import           Data.Foldable (Foldable (..))
import           Data.Text     as T (Text, singleton, unpack)
import qualified Data.Vector   as V
import           Linear.V3     (V3 (..))

instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel = flip Model V.empty . fmap mkChain

        mkChain :: PDB.Chain -> Chain
        mkChain = uncurry Chain . (mkChainName &&& mkChainResidues)

        mkChainName :: PDB.Chain -> Text
        mkChainName = T.singleton . PDB.atomChainID . safeFirstAtom

        mkChainResidues :: PDB.Chain -> V.Vector Residue
        mkChainResidues = V.fromList . fmap mkResidue . flip groupByResidue [] . pure . toList

        -- can be rewritten with sortOn and groupBy
        groupByResidue :: [[PDB.Atom]] -> [PDB.Atom] -> [[PDB.Atom]]
        groupByResidue res []       = res
        groupByResidue [] (x : xs)  = groupByResidue [[x]] xs
        groupByResidue res@(lastList : resultTail) (x : xs)
          | (PDB.atomResSeq x, PDB.atomICode x) == (PDB.atomResSeq (head lastList), PDB.atomICode (head lastList))
                                              = groupByResidue ((x : lastList) : resultTail) xs
          | otherwise                         = groupByResidue ([x] : res) xs

        safeFirstAtom :: V.Vector PDB.Atom -> PDB.Atom
        safeFirstAtom arr | V.length arr > 0 = arr V.! 0
                          | otherwise        = error "Could not pick first atom"


        mkResidue :: [PDB.Atom] -> Residue
        mkResidue []     = error "Cound not make residue from empty list"
        mkResidue atoms' = Residue (PDB.atomResName . head $ atoms')
                                   (V.fromList $ mkAtom <$> atoms')
                                   V.empty   -- now we do not read bonds
                                   Undefined -- now we do not read secondary structure
                                   ""        -- chemical component type?!


        mkAtom :: PDB.Atom -> Atom
        mkAtom PDB.Atom{..} = Atom (coerce atomSerial)
                                   atomName
                                   atomElement
                                   (V3 atomX atomY atomZ)
                                   (read $ T.unpack atomCharge)
                                   atomTempFactor
                                   atomOccupancy
