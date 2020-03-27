{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.PDB
  ( modelsFromPDBText
  , modelsFromPDBFile
  ) where

import qualified Bio.PDB.Type           as PDB
import           Bio.PDB.Reader         (fromFilePDB, fromTextPDB, PDBWarnings)
import           Bio.PDB.BondRestoring  (restoreModelGlobalBonds, restoreChainLocalBonds, residueID)
import           Bio.PDB.Functions      (groupChainByResidue)
import           Bio.Structure

import           Control.Arrow          ((&&&))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (groupBy, sortOn)
import           Data.Coerce            (coerce)
import           Data.Foldable          (Foldable (..))
import           Data.Text              as T (Text, singleton, unpack, pack, strip)
import           Data.Text.IO           as TIO (readFile)
import           Data.Map               (Map)
import qualified Data.Map               as M ((!), fromList)
import qualified Data.Vector            as V
import           Data.Maybe             (fromMaybe)
import           Text.Read              (readMaybe)
import           Linear.V3              (V3 (..))

instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel model = Model (fmap mkChain model) (restoreModelGlobalBonds model)

        mkChain :: PDB.Chain -> Chain
        mkChain = uncurry Chain . (mkChainName &&& mkChainResidues)

        mkChainName :: PDB.Chain -> Text
        mkChainName = T.singleton . PDB.atomChainID . safeFirstAtom

        mkChainResidues :: PDB.Chain -> V.Vector Residue
        mkChainResidues chain = V.fromList . fmap (mkResidue (restoreChainLocalBonds chain)) $ groupChainByResidue chain



        safeFirstAtom :: V.Vector PDB.Atom -> PDB.Atom
        safeFirstAtom arr | V.length arr > 0 = arr V.! 0
                          | otherwise        = error "Could not pick first atom"
        
        mkResidue :: Map Text (V.Vector (Bond LocalID)) -> [PDB.Atom] -> Residue
        mkResidue _ []    = error "Cound not make residue from empty list"
        mkResidue localBondsMap atoms' = Residue (T.strip $ PDB.atomResName firstResidueAtom)
                                                 (PDB.atomResSeq firstResidueAtom)
                                                 (PDB.atomICode firstResidueAtom)
                                                 (V.fromList $ mkAtom <$> atoms')
                                                 (localBondsMap M.! residueID firstResidueAtom)
                                                 Undefined -- now we do not read secondary structure
                                                 ""        -- chemical component type?!
          where
            firstResidueAtom = head atoms'

        mkAtom :: PDB.Atom -> Atom
        mkAtom PDB.Atom{..} = Atom (coerce atomSerial)
                                   (T.strip atomName)
                                   atomElement
                                   (V3 atomX atomY atomZ)
                                   (fromMaybe 0 . readMaybe $ T.unpack atomCharge)
                                   atomTempFactor
                                   atomOccupancy

modelsFromPDBFile :: (MonadIO m) => FilePath -> m (Either Text ([PDBWarnings], V.Vector Model))
modelsFromPDBFile = liftIO . fmap modelsFromPDBText . TIO.readFile

modelsFromPDBText :: Text -> Either Text ([PDBWarnings], V.Vector Model)
modelsFromPDBText pdbText = do
  (warnings, parsedPDB) <- fromTextPDB pdbText
  let models = modelsOf parsedPDB
  pure (warnings, models)