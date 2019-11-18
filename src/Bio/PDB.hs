{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.PDB where

import qualified Bio.PDB.Type           as PDB
import           Bio.Structure

import           Control.Arrow          ((&&&))
import           Data.Foldable          (Foldable (..))
import           Data.Text              as T (Text, singleton, unpack)
import qualified Data.Vector            as V (Vector, empty, fromList, length,
                                              (!))
import           Linear.V3              (V3 (..))

import           Bio.PDB.Parser         (manyModelsP, pdbP, remarkP, titleP)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.Map.Strict        (Map, assocs)
import           Data.Text              (append, intercalate, null, pack)
import qualified Data.Text.IO           as TIO (readFile, writeFile)

instance StructureModels PDB.PDB where
    modelsOf PDB.PDB {..} = fmap mkModel models
      where
        mkModel :: PDB.Model -> Model
        mkModel = Model . fmap mkChain

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
        mkResidue atoms = Residue (PDB.atomResName . head $ atoms)
                                  (V.fromList $ mkAtom <$> atoms)
                                  V.empty   -- now we do not read bonds
                                  Undefined -- now we do not read secondary structure
                                  ""        -- chemical component type?!


        mkAtom :: PDB.Atom -> Atom
        mkAtom PDB.Atom{..} = Atom atomName
                                   atomElement
                                   (V3 atomX atomY atomZ)
                                   (read $ T.unpack atomCharge)
                                   atomTempFactor
                                   atomOccupancy


fromTextChain :: Text -> Either Text [PDB.Model]
fromTextChain = first pack . parseOnly manyModelsP

fromTextTitle :: Text -> Either Text Text
fromTextTitle = first pack . parseOnly titleP


fromTextRemark :: Text -> Either Text (Map PDB.RemarkCode PDB.RemarkData)
fromTextRemark = first pack . parseOnly remarkP


fromTextPDB :: Text -> Either Text PDB.PDB
fromTextPDB = first pack . parseOnly pdbP


fromFilePDB :: MonadIO m => FilePath -> m PDB.PDB
fromFilePDB f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly pdbP

fromFileTitle ::  MonadIO m => FilePath -> m Text
fromFileTitle f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly titleP

interNewLine :: [Text] -> Text
interNewLine = intercalate "\n" . filter (not . Data.Text.null)

mergeData :: [Text] -> Text
mergeData d = foldr (append) "" d

showOtherField :: (PDB.FieldType, PDB.FieldData) -> String
showOtherField (fieldType, fieldData) = show fieldType ++ unpack (mergeData (map (append "\n") (toList fieldData))) ++ "\n\n"

showRemark :: (PDB.RemarkCode, PDB.RemarkData) -> String
showRemark (remarkCode, remarkData) = "REMARK " ++ show remarkCode ++ unpack (mergeData (map (append "\n") (toList remarkData))) ++ "\n\n"

pdbToText :: PDB.PDB -> Text
pdbToText PDB.PDB{..} = interNewLine parts <> "\n"
    where
      parts = [ "TITLE"
              , title
              , "MODELS"
              , pack $ show  models
              , "REMARKS"
              , pack $ foldr (++) "" (map showRemark (assocs remarks))
              , "\nOTHERFIELDS"
              , pack $ foldr (++) "" (map showOtherField (assocs  otherFields))
              ]

toFile :: MonadIO m => PDB.PDB -> FilePath -> m ()
toFile s f = liftIO $ TIO.writeFile f $ pdbToText s


