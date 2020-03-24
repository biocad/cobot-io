{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bio.PDB.BondsRestoring
  ( restoreBonds
  , restoreModelBonds
  ) where

import qualified Bio.PDB.Type as PDB (Atom(..), PDB (..))
import           Bio.Structure (Bond(..), GlobalID(..))

import qualified Data.Vector as V
import           Data.List (groupBy, sortOn, find)
import           Data.Text       (Text)
import qualified Data.Text as T  (strip, pack, unpack)
import           Data.Map.Strict (Map, fromList, lookup, (!))
import           Data.Maybe (maybe)

import           Linear.Metric                    (distance)
import           Linear.V3                        (V3(..))

import           Control.Monad                    (guard)

restoreBonds :: PDB.PDB -> V.Vector (V.Vector (Bond GlobalID))
restoreBonds PDB.PDB{..} = restoreModelBonds <$> models

restoreModelBonds :: V.Vector (V.Vector PDB.Atom) -> V.Vector (Bond GlobalID)
restoreModelBonds chains = V.fromList $ interResidueBonds ++ peptideBonds ++ disulfideBonds
  where
    chainAtomsGroupedByResidue :: V.Vector [[PDB.Atom]]
    chainAtomsGroupedByResidue = groupChainAtomsByResidue <$> chains
    
    interResidueBonds = concatMap restoreChainInterResidueBonds chainAtomsGroupedByResidue
    peptideBonds = concatMap restoreChainPeptideBonds chainAtomsGroupedByResidue
    disulfideBonds = restoreDisulfideBonds . concat $ V.toList chainAtomsGroupedByResidue

restoreDisulfideBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreDisulfideBonds atomsGroupedByResidues = do
  atom1 <- cystineSulfur
  atom2 <- cystineSulfur
  guard (PDB.atomSerial atom1 < PDB.atomSerial atom2)
  guard $ distance (coords atom1) (coords atom2) < sulfidicBondMaxLength
  pure $ Bond (GlobalID $ PDB.atomSerial atom1) (GlobalID $ PDB.atomSerial atom2) 1
  where
    bonds = []
    cystineSulfur :: [PDB.Atom]
    cystineSulfur = filter (("SG" ==) . T.strip . PDB.atomName) $ concat cystines
    cystines :: [[PDB.Atom]]
    cystines = filter cystinePredicate atomsGroupedByResidues
    cystinePredicate :: [PDB.Atom] -> Bool
    cystinePredicate residue = any (("SG" ==) . T.strip . PDB.atomName) residue && all (("HG" /=) . T.strip . PDB.atomName) residue
    coords :: PDB.Atom -> V3 Float
    coords PDB.Atom{..} = V3 atomX atomY atomZ

sulfidicBondMaxLength :: Float
sulfidicBondMaxLength = 2.56

restoreChainPeptideBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreChainPeptideBonds atomsGroupedByResidue = restoreChainPeptideBonds' atomsGroupedByResidue []
  where
    restoreChainPeptideBonds' :: [[PDB.Atom]] -> [Bond GlobalID] -> [Bond GlobalID]
    restoreChainPeptideBonds' [] acc = acc
    restoreChainPeptideBonds' (_:[]) acc = acc
    restoreChainPeptideBonds' (residue1:residue2:residues) acc = 
      restoreChainPeptideBonds' (residue2:residues) (constructBond residue1 residue2 : acc)

    constructBond :: [PDB.Atom] -> [PDB.Atom] -> Bond GlobalID
    constructBond residue1 residue2 = Bond (GlobalID $ getAtomIndex residue1 "C") (GlobalID $ getAtomIndex residue2 "N") 1
    getAtomIndex :: [PDB.Atom] -> Text -> Int
    getAtomIndex atoms atomNameToFind = case find ((atomNameToFind ==) . T.strip . PDB.atomName) atoms of 
      Just PDB.Atom{..} -> atomSerial
      Nothing           -> error ("Atom with name " ++ T.unpack atomNameToFind ++ " wasn't found in residue " ++ residueId atoms ++ ", chain: " ++ chainId atoms)
    residueId :: [PDB.Atom] -> String
    residueId (PDB.Atom{..}:_) = T.unpack atomResName ++ show atomResSeq ++ show atomICode
    chainId :: [PDB.Atom] -> String
    chainId (PDB.Atom{..}:_) = show atomChainID
    

restoreChainInterResidueBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreChainInterResidueBonds = concatMap restoreIntraResidueBonds

restoreIntraResidueBonds :: [PDB.Atom] -> [Bond GlobalID]
restoreIntraResidueBonds residueAtoms = map constructBond residueBonds
  where
    -- TODO: use safe 'lookup' operation and throw errors with clear messages
    -- TODO: support bond order somehow
    constructBond :: (Text, Text) -> Bond GlobalID
    constructBond (fromAtomName, toAtomName) = Bond (GlobalID $ atomNameToIndex ! fromAtomName) (GlobalID $ atomNameToIndex ! toAtomName) 1
    atomNameToIndex :: Map Text Int
    atomNameToIndex = fromList $ (\PDB.Atom{..} -> (atomName, atomSerial)) <$> residueAtoms
    residueBonds :: [(Text, Text)]
    residueBonds = intraResidueBonds . T.strip . PDB.atomResName $ head residueAtoms

intraResidueBonds :: Text -> [(Text, Text)]
intraResidueBonds "NMA" = [("CA", "N")]
intraResidueBonds "ACE" = [("C", "O"), ("C", "CH3")]
intraResidueBonds residueName = [("N", "CA"), ("CA", "C"), ("C", "O")] ++ caCbBond residueName ++ sideChainBonds residueName

caCbBond :: Text -> [(Text, Text)]
caCbBond aminoacid = case aminoacid of
  "GLY" -> []
  _     -> [("CA", "CB")]

--TODO: don't forget to check what is happening on terminal residues
--TODO: don't forget about HIE, HID, etc.
sideChainBonds :: Text -> [(Text, Text)]
sideChainBonds "ALA" = [] -- CB [HB1, HB2, HB3]
sideChainBonds "GLY" = [] -- nothing
sideChainBonds "VAL" = [("CB", "CG1"), ("CB", "CG2")] ++ bwhMany [("CB", ["HB"]), ("CG1", ["HG11", "HG12", "HG13"]), ("CG2", ["HG21", "HG22", "HG23"])]
sideChainBonds "ILE" = [("CB", "CG1"), ("CB", "CG2"), ("CG1", "CD1")] ++ bwhMany [("CB", ["HB"]), ("CG1", ["HG13", "HG12"]), ("CG2", ["HG21", "HG22", "HG23"]), ("CD1", ["HD11", "HD12", "HD13"])]
sideChainBonds "LEU" = [("CB", "CG") , ("CG", "CD1"), ("CG", "CD2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG"]), ("CD1", ["HD11", "HD12", "HD13"]), ("CD2", ["HD21", "HD22", "HD23"])]
sideChainBonds "MET" = [("CB", "CG") , ("CG", "SD") , ("SD", "CE")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CE", ["HE1", "HE2", "HE3"])]
sideChainBonds "PHE" = [("CB", "CG") , ("CG", "CD1"), ("CD1", "CE1"), ("CE1", "CZ") , ("CZ", "CE2") , ("CE2", "CD2"), ("CD2", "CG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("CE1", ["HE1"]), ("CZ", ["HZ"]), ("CE2", ["HE2"]), ("CD2", ["HD2"])]
sideChainBonds "TYR" = [("CB", "CG") , ("CG", "CD1"), ("CD1", "CE1"), ("CE1", "CZ") , ("CZ", "CE2") , ("CE2", "CD2"), ("CD2", "CG") , ("CZ", "OH")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("CE1", ["HE1"]), ("CE2", ["HE2"]), ("CD2", ["HD2"]), ("OH", ["HH"])]
sideChainBonds "TRP" = [("CB", "CG") , ("CG", "CD1"), ("CD1", "NE1"), ("NE1", "CE2"), ("CE2", "CD2"), ("CD2", "CG") , ("CD2", "CE3"), ("CE3", "CZ3"), ("CZ3", "CH2"), ("CH2", "CZ2"), ("CZ2", "CE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("NE1", ["HE1"]), ("CE3", ["HE3"]), ("CZ3", ["HZ3"]), ("CH2", ["HH2"]), ("CZ2", ["HZ2"])]
sideChainBonds "SER" = [("CB", "OG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("OG", ["HG"])]
sideChainBonds "THR" = [("CB", "OG1"), ("CB", "CG2")] ++ bwhMany [("CB", ["HB"]), ("OG1", ["HG1"]), ("CG2", ["HG21", "HG22", "HG23"])]
sideChainBonds "ASN" = [("CB", "CG") , ("CG", "OD1"), ("CG", "ND2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("ND2", ["HD22", "HD21"])]
sideChainBonds "GLN" = [("CB", "CG") , ("CG", "CD") , ("CD", "OE1") , ("CD", "NE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("NE2", ["HE22", "HE21"])]
sideChainBonds "CYS" = [("CB", "SG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("SG", ["HG"])]
sideChainBonds "PRO" = [("CB", "CG") , ("CG", "CD") , ("CD", "N")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD2", "HD3"])]
sideChainBonds "ARG" = [("CB", "CG") , ("CG", "CD") , ("CD", "NE")  , ("NE", "CZ")  , ("CZ", "NH2") , ("CZ", "NH1")] ++ bwhMany[("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD3", "HD2"]), ("NE", ["HE"]), ("NH1", ["HH12", "HH11"]), ("NH2", ["HH22", "HH21"])]
sideChainBonds "HIS" = [("CB", "CG") , ("CG", "ND1"), ("ND1", "CE1"), ("CE1", "NE2"), ("NE2", "CD2"), ("CD2", "CG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("ND1", ["HD1"]), ("CE1", ["HE1"]), ("NE2", ["HE2"]), ("CD2", ["HD2"])]
sideChainBonds "LYS" = [("CB", "CG") , ("CG", "CD") , ("CD", "CE")  , ("CE", "NZ")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD3", "HD2"]), ("CE", ["HE3", "HE2"]), ("NZ", ["HZ1", "HZ2", "HZ3"])]
sideChainBonds "ASP" = [("CB", "CG") , ("CG", "OD1"), ("CG", "OD2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("OD2", ["HD2"])]
sideChainBonds "GLU" = [("CB", "CG") , ("CG", "CD") , ("CD", "OE1") , ("CD", "OE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("OE2", ["HE2"])]

bwhMany :: [(Text, [Text])] -> [(Text, Text)]
bwhMany = concatMap bwh

bwh :: (Text, [Text]) -> [(Text, Text)]
bwh = heavyAtomBondsWithHydrogens

heavyAtomBondsWithHydrogens :: (Text, [Text]) -> [(Text, Text)]
heavyAtomBondsWithHydrogens (heavyAtomName, hydrogenNames) = (heavyAtomName,) <$> hydrogenNames


groupChainAtomsByResidue :: V.Vector PDB.Atom -> [[PDB.Atom]]
groupChainAtomsByResidue = sortOn (sortOnResidue . head) . groupBy atomsFromSameResidue . V.toList
  where 
    atomsFromSameResidue :: PDB.Atom -> PDB.Atom -> Bool
    atomsFromSameResidue atom1 atom2 = PDB.atomResSeq atom1 == PDB.atomResSeq atom2 && PDB.atomICode atom1 == PDB.atomICode atom2
    sortOnResidue :: PDB.Atom -> Int
    sortOnResidue PDB.Atom{..} = atomSerial * 100 + (insertionCodeSortingCorrections ! atomICode)
    insertionCodeSortingCorrections :: Map Char Int
    insertionCodeSortingCorrections = fromList $ zip (' ':['A'..'Z']) [0..]
