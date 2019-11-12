module Bio.PDB
  ( module PDB
  ) where

import qualified Bio.PDB.Type  as PDB
import           Bio.Structure

import           Control.Arrow ((&&&))
import           Data.Array    (Array, listArray, (!))
import           Data.Foldable (Foldable (..))
import           Data.Text     as T (Text, singleton, unpack)
import           Linear.V3     (V3 (..))

instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel = Model . fmap mkChain

        mkChain :: PDB.Chain -> Chain
        mkChain = uncurry Chain . (mkChainName &&& mkChainResidues)

        mkChainName :: PDB.Chain -> Text
        mkChainName = T.singleton . PDB.atomChainID . safeFirstAtom

        mkChainResidues :: PDB.Chain -> Array Int Residue
        mkChainResidues = l2a . fmap mkResidue . flip groupByResidue [] . pure . toList

        -- can be rewritten with sortOn and groupBy
        groupByResidue :: [[PDB.Atom]] -> [PDB.Atom] -> [[PDB.Atom]]
        groupByResidue res []       = res
        groupByResidue [] (x : xs)  = groupByResidue [[x]] xs
        groupByResidue res@(lastList : resultTail) (x : xs)
          | (PDB.atomResSeq x, PDB.atomICode x) == (PDB.atomResSeq (head lastList), PDB.atomICode (head lastList))
                                              = groupByResidue ((x : lastList) : resultTail) xs
          | otherwise                         = groupByResidue ([x] : res) xs

        safeFirstAtom :: Array Int PDB.Atom -> PDB.Atom
        safeFirstAtom arr | length arr > 0 = arr ! 0
                          | otherwise      = error "Could not pick first atom"


        mkResidue :: [PDB.Atom] -> Residue
        mkResidue []    = error "Cound not make residue from empty list"
        mkResidue atoms = Residue (PDB.atomResName . head $ atoms)
                                  (l2a $ mkAtom <$> atoms)
                                  zeroArray -- now we do not read bonds
                                  Undefined -- now we do not read secondary structure
                                  ""        -- chemical component type?!

        zeroArray :: Array Int a
        zeroArray = listArray (0,0) []

        mkAtom :: PDB.Atom -> Atom
        mkAtom PDB.Atom{..} = Atom atomName
                                   atomElement
                                   (V3 atomX atomY atomZ)
                                   (read $ T.unpack atomCharge)
                                   atomTempFactor
                                   atomOccupancy

l2a :: [a] -> Array Int a
l2a lst = listArray (0, length lst - 1) lst
