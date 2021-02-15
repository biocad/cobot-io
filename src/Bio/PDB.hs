{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.PDB
  ( modelsFromPDBText, modelsToPDBText
  , modelsFromPDBFile, modelsToPDBFile
  ) where

import           Bio.PDB.BondRestoring  (residueID,
                                         restoreChainLocalBonds,
                                         restoreModelGlobalBonds,
                                         toAtomPDBId,
                                         AtomPDBId)
import           Bio.PDB.Functions      (groupChainByResidue)
import           Bio.PDB.Reader         (PDBWarnings, fromTextPDB)
import qualified Bio.PDB.Type           as PDB
import           Bio.PDB.Writer         (pdbToFile, pdbToText)
import           Bio.Structure
import           Control.Arrow          ((&&&))
import           Control.Lens           ((^.))
import           Control.Monad          (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (sort)
import           Data.Map               (Map)
import qualified Data.Map               as M (fromList, (!))
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T (head, pack, singleton, strip,
                                              unpack)
import           Data.Text.IO           as TIO (readFile)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Linear.V3              (V3 (..), _x, _y, _z)
import           Text.Read              (readMaybe)

instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel model = case length atomPDBIdToNilBasedIndex == length allModelAtomPDBIds of
            False -> error "Mapping from PDB id to nil based index must be a bijection."
            True  -> Model (fmap mkChain model) (restoreModelGlobalBonds atomPDBIdToNilBasedIndex model)
          where
            -- We cannot use only atomSerial as key because there 
            -- can be two atoms with the same atomSerial in different chains
            atomPDBIdToNilBasedIndex :: Map AtomPDBId Int
            atomPDBIdToNilBasedIndex = M.fromList $ allModelAtomPDBIds `zip` [0..]

            allModelAtomPDBIds :: [AtomPDBId]
            allModelAtomPDBIds = sort . V.toList . fmap toAtomPDBId . V.concat $ V.toList model

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
            mkAtom atom@PDB.Atom{..} = Atom (GlobalID $ atomPDBIdToNilBasedIndex M.! toAtomPDBId atom)
                                       atomSerial
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

instance StructureSerializable PDB.PDB where
  serializeModels models = PDB.PDB "Serialized model" pdbModels mempty mempty
    where
      pdbModels = fmap toPDBModel models

      toPDBModel :: Model -> PDB.Model
      toPDBModel = fmap toPDBChain . modelChains

      toPDBChain :: Chain -> PDB.Chain
      toPDBChain ch = fmap toPDBAtom . join $ (\r -> fmap ((,,) ch r) $ resAtoms r) <$> chainResidues ch

      toPDBAtom :: (Chain, Residue, Atom) -> PDB.Atom
      toPDBAtom (Chain{..}, Residue{..}, Atom{..}) = res
        where
          res =
            PDB.Atom
              (getGlobalID atomId + 1)
              atomName
              nullAltLoc
              resName
              (T.head chainName)
              resNumber
              resInsertionCode
              (atomCoords ^. _x)
              (atomCoords ^. _y)
              (atomCoords ^. _z)
              occupancy
              bFactor
              atomElement
              (T.pack $ show formalCharge)

          nullAltLoc :: Char
          nullAltLoc = ' '

-- | Writes models to the given path as PDB.
--
modelsToPDBFile :: MonadIO m => Vector Model -> FilePath -> m ()
modelsToPDBFile models = pdbToFile $ serializeModels models

-- | Converts models to their 'Text' representation as PDB.
--
modelsToPDBText :: Vector Model -> Text
modelsToPDBText = pdbToText . serializeModels
