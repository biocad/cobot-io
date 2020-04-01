module Bio.PDB.Functions
  ( groupChainByResidue
  ) where

import qualified Bio.PDB.Type as PDB (Atom (..))
import           Data.Map            (Map)
import qualified Data.Map as M       (fromList, (!))
import           Data.List           (groupBy, sortOn)
import           Data.Vector         (Vector)
import qualified Data.Vector as V    (toList)

groupChainByResidue :: Vector PDB.Atom -> [[PDB.Atom]]
groupChainByResidue = sortOn (sortOnResidue . head) . groupBy atomsFromSameResidue . V.toList
  where 
    atomsFromSameResidue :: PDB.Atom -> PDB.Atom -> Bool
    atomsFromSameResidue atom1 atom2 = PDB.atomResSeq atom1 == PDB.atomResSeq atom2 && PDB.atomICode atom1 == PDB.atomICode atom2
    sortOnResidue :: PDB.Atom -> Int
    sortOnResidue PDB.Atom{..} = atomSerial * 100 + (insertionCodeSortingCorrections M.! atomICode)
    insertionCodeSortingCorrections :: Map Char Int
    insertionCodeSortingCorrections = M.fromList $ zip (' ':['A'..'Z']) [0..]
