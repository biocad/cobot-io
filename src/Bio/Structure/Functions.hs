module Bio.Structure.Functions
  ( filterAtomsOfModel
  ) where

import           Bio.Structure   (Atom (..), Bond (..), Chain (..),
                                  GlobalID (..), LocalID (..), Model (..),
                                  Residue (..))
import qualified Data.Map.Strict as M (fromList, (!))
import           Data.Set        (Set)
import qualified Data.Set        as S (fromList, notMember, unions)
import           Data.Vector     (Vector)
import qualified Data.Vector     as V (filter, fromList, length, toList, unzip)

-- | Takes predicate on 'Atom's of 'Model' and returns new 'Model' containing only atoms
--   satisfying given predicate.
--
filterAtomsOfModel :: (Atom -> Bool) -> Model -> Model
filterAtomsOfModel p Model{..} = Model newChains newBonds
  where
    removePred         = not . p
    (newChains, indss) = V.unzip $ fmap (removeAtomsFromChain removePred) modelChains

    inds     = S.unions $ V.toList indss
    newBonds = V.filter (\(Bond l r _) -> l `S.notMember` inds && r `S.notMember` inds) modelBonds

removeAtomsFromChain :: (Atom -> Bool) -> Chain -> (Chain, Set GlobalID)
removeAtomsFromChain p Chain{..} = (Chain chainName newResidues, S.unions $ V.toList indss)
  where
    (newResidues, indss) = V.unzip $ fmap (removeAtomsFromResidue p) chainResidues

removeAtomsFromResidue :: (Atom -> Bool) -> Residue -> (Residue, Set GlobalID)
removeAtomsFromResidue p r'@Residue{..} = (res, S.fromList $ V.toList $ fmap atomId withAtom)
  where
    (withAtom, withoutAtom, indsToDelete) = partitionAndInds resAtoms

    oldIndsToNew = M.fromList $ fmap (\i -> (i, newInd i)) [0 .. V.length resAtoms - 1]
    newBonds     = fmap modifyBond $ V.filter leaveBond resBonds

    res = r' { resAtoms=withoutAtom, resBonds=newBonds }

    leaveBond :: Bond LocalID -> Bool
    leaveBond (Bond (LocalID l) (LocalID r) _) = l `notElem` indsToDelete && r `notElem` indsToDelete

    modifyBond :: Bond LocalID -> Bond LocalID
    modifyBond (Bond (LocalID l) (LocalID r) t) = Bond (LocalID $ oldIndsToNew M.! l)
                                                       (LocalID $ oldIndsToNew M.! r)
                                                       t

    newInd :: Int -> Int
    newInd i = i - (length $ filter (< i) indsToDelete)

    partitionAndInds :: Vector Atom -> (Vector Atom, Vector Atom, [Int])
    partitionAndInds = go 0 ([], [], []) . V.toList
      where
        go :: Int -> ([Atom], [Atom], [Int]) -> [Atom] -> (Vector Atom, Vector Atom, [Int])
        go _ (sat, notSat, inds) []       = (V.fromList $ reverse sat, V.fromList $ reverse notSat, reverse inds)
        go i (sat, notSat, inds) (x : xs) = go (i + 1) newState xs
          where
            newState = if p x then (x : sat, notSat, i : inds) else (sat, x : notSat, inds)
