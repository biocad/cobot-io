{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.PDB
  ( modelsFromPDBText
  , modelsFromPDBFile
  ) where

import qualified Bio.PDB.Type           as PDB
import           Bio.PDB.Reader         (fromFilePDB, fromTextPDB, PDBWarnings)
import           Bio.PDB.BondRestoring  (restoreModelBonds)
import           Bio.Structure

import           Control.Arrow          ((&&&))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Coerce            (coerce)
import           Data.Foldable          (Foldable (..))
import           Data.Text              as T (Text, singleton, unpack, pack, strip)
import           Data.Text.IO           as TIO (readFile)
import qualified Data.Vector            as V
import           Linear.V3              (V3 (..))

-- TODO: bonds recovering must have been done here
-- TODO: write tests for this conversion
instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel model = Model (fmap mkChain model) (restoreModelBonds model)

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
        mkResidue []    = error "Cound not make residue from empty list"
        mkResidue atoms = Residue (T.strip $ PDB.atomResName firstResidueAtom)
                                  (PDB.atomResSeq firstResidueAtom)
                                  (PDB.atomICode firstResidueAtom)
                                  (V.fromList $ mkAtom <$> atoms)
                                  V.empty   -- now we do not read bonds
                                  Undefined -- now we do not read secondary structure
                                  ""        -- chemical component type?!
          where
            firstResidueAtom = head atoms


        mkAtom :: PDB.Atom -> Atom
        mkAtom PDB.Atom{..} = Atom (coerce atomSerial)
                                   (T.strip atomName)
                                   atomElement
                                   (V3 atomX atomY atomZ)
                                   (read $ T.unpack atomCharge)
                                   atomTempFactor
                                   atomOccupancy

modelsFromPDBFile :: (MonadIO m) => FilePath -> m (Either Text ([PDBWarnings], V.Vector Model))
modelsFromPDBFile = liftIO . fmap modelsFromPDBText . TIO.readFile

modelsFromPDBText :: Text -> Either Text ([PDBWarnings], V.Vector Model)
modelsFromPDBText pdbText = do
  (warnings, parsedPDB) <- fromTextPDB pdbText
  let models = modelsOf parsedPDB
  pure (warnings, models)