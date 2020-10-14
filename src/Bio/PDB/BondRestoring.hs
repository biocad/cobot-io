{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bio.PDB.BondRestoring
  ( restoreModelGlobalBonds
  , restoreModelLocalBonds
  , restoreChainLocalBonds
  , residueID
  ) where

import qualified Bio.PDB.Type as PDB  (Atom(..))
import           Bio.PDB.Functions    (groupChainByResidue)
import           Bio.Structure        (Bond (..), GlobalID (..), LocalID (..))

import           Data.Vector          (Vector)
import qualified Data.Vector as V     (fromList, toList)
import           Data.List            (find, sort)
import           Data.Text            (Text)
import qualified Data.Text as T       (strip, pack, unpack)
import           Data.Map.Strict      (Map, (!?), (!))
import qualified Data.Map.Strict as M (fromList)
import           Data.Maybe           (catMaybes)

import           Linear.Metric        (distance)
import           Linear.V3            (V3(..))

import           Control.Monad        (guard)

residueID :: PDB.Atom -> Text
residueID PDB.Atom{..} = T.pack (show atomChainID) <> T.pack (show atomResSeq) <> T.pack (show atomICode)

restoreModelLocalBonds :: Vector (Vector PDB.Atom) -> Map Text (Vector (Bond LocalID))
restoreModelLocalBonds = M.fromList . concatMap restoreChainLocalBonds'

restoreChainLocalBonds :: Vector PDB.Atom -> Map Text (Vector (Bond LocalID))
restoreChainLocalBonds = M.fromList . restoreChainLocalBonds'

restoreChainLocalBonds' :: Vector PDB.Atom -> [(Text, Vector (Bond LocalID))]
restoreChainLocalBonds' chainAtoms = residueIDToLocalBonds
  where
    residueIDToLocalBonds :: [(Text, Vector (Bond LocalID))]
    residueIDToLocalBonds = do
      (residueAtoms, residueBonds) <- zip chainAtomsGroupedByResidue intraResidueGlobalBonds
      let localBonds = V.fromList $ convertGlobalsToLocals residueAtoms residueBonds
      let _residueID = residueID $ head residueAtoms
      pure (_residueID, localBonds)
    
    intraResidueGlobalBonds :: [[Bond GlobalID]]
    intraResidueGlobalBonds = fmap restoreIntraResidueBonds chainAtomsGroupedByResidue
    
    chainAtomsGroupedByResidue :: [[PDB.Atom]]
    chainAtomsGroupedByResidue = groupChainByResidue chainAtoms
    
    convertGlobalsToLocals :: [PDB.Atom] -> [Bond GlobalID] -> [Bond LocalID]
    convertGlobalsToLocals residueAtoms = map convertGlobalToLocal
      where
        convertGlobalToLocal :: Bond GlobalID -> Bond LocalID
        convertGlobalToLocal (Bond (GlobalID from) (GlobalID to) order) = 
          Bond (LocalID $ globalToLocalIdxMap ! from) (LocalID $ globalToLocalIdxMap ! to) order
        
        globalToLocalIdxMap :: Map Int Int
        globalToLocalIdxMap = M.fromList $ zip sortedGlobalIndices [0..]
        
        sortedGlobalIndices :: [Int]
        sortedGlobalIndices = map PDB.atomSerial $ sort residueAtoms


restoreModelGlobalBonds :: Map Int Int -> Vector (Vector PDB.Atom) -> Vector (Bond GlobalID)
restoreModelGlobalBonds atomSerialToNilBasedIndex chains = convertGlobalIDs atomSerialToNilBasedIndex . V.fromList $ _intraResidueBonds ++ peptideBonds ++ disulfideBonds
  where
    convertGlobalIDs :: Map Int Int -> Vector (Bond GlobalID) -> Vector (Bond GlobalID)
    convertGlobalIDs mapping = reindexBonds (\(GlobalID v) -> GlobalID $ mapping ! v)
    
    reindexBonds :: (a -> a) -> Vector (Bond a) -> Vector (Bond a)
    reindexBonds convertID = fmap (\(Bond from to order) -> Bond (convertID from) (convertID to) order)

    chainAtomsGroupedByResidue :: Vector [[PDB.Atom]]
    chainAtomsGroupedByResidue = fmap groupChainByResidue chains
    
    _intraResidueBonds :: [Bond GlobalID]
    _intraResidueBonds = concatMap restoreChainIntraResidueBonds chainAtomsGroupedByResidue
    
    peptideBonds :: [Bond GlobalID]
    peptideBonds = concatMap restoreChainPeptideBonds chainAtomsGroupedByResidue
    
    disulfideBonds :: [Bond GlobalID]
    disulfideBonds = restoreDisulfideBonds . concat $ V.toList chainAtomsGroupedByResidue

restoreDisulfideBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreDisulfideBonds atomsGroupedByResidue = do
  atom1 <- cystineSulfur
  atom2 <- cystineSulfur
  guard (PDB.atomSerial atom1 < PDB.atomSerial atom2)
  guard $ distance (coords atom1) (coords atom2) < sulfidicBondMaxLength
  pure $ Bond (GlobalID $ PDB.atomSerial atom1) (GlobalID $ PDB.atomSerial atom2) 1
    where
      cystineSulfur :: [PDB.Atom]
      cystineSulfur = filter (("SG" ==) . T.strip . PDB.atomName) $ concat cystines
      cystines :: [[PDB.Atom]]
      cystines = filter cystinePredicate atomsGroupedByResidue
      cystinePredicate :: [PDB.Atom] -> Bool
      cystinePredicate residue = any (("SG" ==) . T.strip . PDB.atomName) residue && all (("HG" /=) . T.strip . PDB.atomName) residue

coords :: PDB.Atom -> V3 Float
coords PDB.Atom{..} = V3 atomX atomY atomZ

sulfidicBondMaxLength :: Float
sulfidicBondMaxLength = 2.56

peptideBondMaxLength :: Float
peptideBondMaxLength = 1.5

restoreChainPeptideBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreChainPeptideBonds atomsGroupedByResidue = catMaybes $ restoreChainPeptideBonds' atomsGroupedByResidue mempty
  where
    restoreChainPeptideBonds' :: [[PDB.Atom]] -> [Maybe (Bond GlobalID)] -> [Maybe (Bond GlobalID)]
    restoreChainPeptideBonds' [] acc = acc
    restoreChainPeptideBonds' [_] acc = acc
    restoreChainPeptideBonds' (residue1:residue2:residues) acc = 
      restoreChainPeptideBonds' (residue2:residues) (constructBond residue1 residue2 : acc)

    constructBond :: [PDB.Atom] -> [PDB.Atom] -> Maybe (Bond GlobalID)
    constructBond residue1 residue2 = do
        let carbonAtom1   = getAtomByName residue1 "C"
            nitrogenAtom2 = getAtomByName residue2 "N"

        -- check if the atoms are close enough
        -- in order not to restore a wrong peptide bond in case of absent residues (gaps)
        guard $ distance (coords carbonAtom1) (coords nitrogenAtom2) < peptideBondMaxLength

        pure $ Bond (GlobalID $ PDB.atomSerial carbonAtom1) (GlobalID $ PDB.atomSerial nitrogenAtom2) 1
    
    getAtomByName :: [PDB.Atom] -> Text -> PDB.Atom
    getAtomByName atoms atomNameToFind = case find ((atomNameToFind ==) . T.strip . PDB.atomName) atoms of 
      Just a  -> a
      Nothing -> error ("Atom with name " ++ T.unpack atomNameToFind ++ " wasn't found in residue " ++ residueId atoms ++ ", chain: " ++ chainId atoms)
    
    residueId :: [PDB.Atom] -> String
    residueId [] = error "cobot-io: it's impossible to form a residue ID on a residue with no atoms"
    residueId (PDB.Atom{..}:_) = T.unpack atomResName ++ show atomResSeq ++ show atomICode
    
    chainId :: [PDB.Atom] -> String
    chainId [] = error "cobot-io: it's impossible to get a chain ID on a chain with no atoms"
    chainId (PDB.Atom{..}:_) = show atomChainID
    

restoreChainIntraResidueBonds :: [[PDB.Atom]] -> [Bond GlobalID]
restoreChainIntraResidueBonds = concatMap restoreIntraResidueBonds

restoreIntraResidueBonds :: [PDB.Atom] -> [Bond GlobalID]
restoreIntraResidueBonds residueAtoms = catMaybes $ constructBond <$> residueBonds
  where
    -- TODO: support bond order somehow
    constructBond :: (Text, Text) -> Maybe (Bond GlobalID)
    constructBond (fromAtomName, toAtomName) = Bond <$> constructGlobalID fromAtomName <*> constructGlobalID  toAtomName <*> Just 1
    
    constructGlobalID :: Text -> Maybe GlobalID
    constructGlobalID atomName = GlobalID <$> atomNameToIndex !? atomName
    
    atomNameToIndex :: Map Text Int
    atomNameToIndex = M.fromList $ (\PDB.Atom{..} -> (T.strip atomName, atomSerial)) <$> residueAtoms
    
    residueBonds :: [(Text, Text)]
    residueBonds = intraResidueBonds . T.strip . PDB.atomResName $ head residueAtoms

intraResidueBonds :: Text -> [(Text, Text)]
intraResidueBonds "NMA" = [("CA", "N")]
intraResidueBonds "ACE" = [("C", "O"), ("C", "CH3")]
intraResidueBonds residueName = backboneBonds ++ caCbBonds residueName ++ sideChainBonds residueName

backboneBonds :: [(Text, Text)]
backboneBonds = [("N", "CA"), ("CA", "C"), ("C", "O"), ("N", "H")] ++ [("C","OXT"), ("C","HXT")] ++ bwhMany [("N", ["H1", "H2", "H3"])]

caCbBonds :: Text -> [(Text, Text)]
caCbBonds aminoacid = case aminoacid of
  "GLY" -> bwhMany [("CA", ["HA2", "HA3"])]
  _     -> [("CA", "CB"), ("CA", "HA")]

sideChainBonds :: Text -> [(Text, Text)]
sideChainBonds "ALA" = bwhMany [("CB", ["HB1", "HB2", "HB3"])]
sideChainBonds "ARG" = [("CB", "CG"), ("CG", "CD"), ("CD", "NE"), ("NE", "CZ"), ("CZ", "NH2"), ("CZ", "NH1")] ++ bwhMany[("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD3", "HD2"]), ("NE", ["HE"]), ("NH1", ["HH12", "HH11"]), ("NH2", ["HH22", "HH21"])]
sideChainBonds "ASN" = [("CB", "CG"), ("CG", "OD1"), ("CG", "ND2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("ND2", ["HD22", "HD21"])]
sideChainBonds "ASP" = [("CB", "CG"), ("CG", "OD1"), ("CG", "OD2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("OD2", ["HD2"])]
sideChainBonds "CYS" = [("CB", "SG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("SG", ["HG"])]
sideChainBonds "GLN" = [("CB", "CG"), ("CG", "CD"), ("CD", "OE1"), ("CD", "NE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("NE2", ["HE22", "HE21"])]
sideChainBonds "GLU" = [("CB", "CG"), ("CG", "CD"), ("CD", "OE1"), ("CD", "OE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("OE2", ["HE2"])]
sideChainBonds "GLY" = [] -- nothing
sideChainBonds "HIS" = [("CB", "CG"), ("CG", "ND1"), ("ND1", "CE1"), ("CE1", "NE2"), ("NE2", "CD2"), ("CD2", "CG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("ND1", ["HD1"]), ("CE1", ["HE1"]), ("NE2", ["HE2"]), ("CD2", ["HD2"])]
sideChainBonds "ILE" = [("CB", "CG1"), ("CB", "CG2"), ("CG1", "CD1")] ++ bwhMany [("CB", ["HB"]), ("CG1", ["HG13", "HG12"]), ("CG2", ["HG21", "HG22", "HG23"]), ("CD1", ["HD11", "HD12", "HD13"])]
sideChainBonds "LEU" = [("CB", "CG"), ("CG", "CD1"), ("CG", "CD2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG"]), ("CD1", ["HD11", "HD12", "HD13"]), ("CD2", ["HD21", "HD22", "HD23"])]
sideChainBonds "LYS" = [("CB", "CG"), ("CG", "CD"), ("CD", "CE"), ("CE", "NZ")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD3", "HD2"]), ("CE", ["HE3", "HE2"]), ("NZ", ["HZ1", "HZ2", "HZ3"])]
sideChainBonds "MET" = [("CB", "CG"), ("CG", "SD"), ("SD", "CE")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CE", ["HE1", "HE2", "HE3"])]
sideChainBonds "PHE" = [("CB", "CG"), ("CG", "CD1"), ("CD1", "CE1"), ("CE1", "CZ"), ("CZ", "CE2"), ("CE2", "CD2"), ("CD2", "CG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("CE1", ["HE1"]), ("CZ", ["HZ"]), ("CE2", ["HE2"]), ("CD2", ["HD2"])]
sideChainBonds "PRO" = [("CB", "CG"), ("CG", "CD"), ("CD", "N")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CG", ["HG3", "HG2"]), ("CD", ["HD2", "HD3"])]
sideChainBonds "SER" = [("CB", "OG")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("OG", ["HG"])]
sideChainBonds "THR" = [("CB", "OG1"), ("CB", "CG2")] ++ bwhMany [("CB", ["HB"]), ("OG1", ["HG1"]), ("CG2", ["HG21", "HG22", "HG23"])]
sideChainBonds "TRP" = [("CB", "CG"), ("CG", "CD1"), ("CD1", "NE1"), ("NE1", "CE2"), ("CE2", "CD2"), ("CD2", "CG"), ("CD2", "CE3"), ("CE3", "CZ3"), ("CZ3", "CH2"), ("CH2", "CZ2"), ("CZ2", "CE2")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("NE1", ["HE1"]), ("CE3", ["HE3"]), ("CZ3", ["HZ3"]), ("CH2", ["HH2"]), ("CZ2", ["HZ2"])]
sideChainBonds "TYR" = [("CB", "CG"), ("CG", "CD1"), ("CD1", "CE1"), ("CE1", "CZ"), ("CZ", "CE2"), ("CE2", "CD2"), ("CD2", "CG"), ("CZ", "OH")] ++ bwhMany [("CB", ["HB3", "HB2"]), ("CD1", ["HD1"]), ("CE1", ["HE1"]), ("CE2", ["HE2"]), ("CD2", ["HD2"]), ("OH", ["HH"])]
sideChainBonds "VAL" = [("CB", "CG1"), ("CB", "CG2")] ++ bwhMany [("CB", ["HB"]), ("CG1", ["HG11", "HG12", "HG13"]), ("CG2", ["HG21", "HG22", "HG23"])]
sideChainBonds unknownResidue = error . T.unpack $ "cobot-io: we don't know what to do with residue " <> unknownResidue

bwhMany :: [(Text, [Text])] -> [(Text, Text)]
bwhMany = concatMap bwh

bwh :: (Text, [Text]) -> [(Text, Text)]
bwh = heavyAtomBondsWithHydrogens

heavyAtomBondsWithHydrogens :: (Text, [Text]) -> [(Text, Text)]
heavyAtomBondsWithHydrogens (heavyAtomName, hydrogenNames) = (heavyAtomName,) <$> hydrogenNames
