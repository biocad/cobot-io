{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bio.PDB where
-- ( 
--   fromTextChain
--   , fromFile
--   , CoordLike (..)
--   , fromTextTitle
--   , fromTextRemark
--   , fromTextHeader
--   -- , toFile
--   -- , fromText
--   -- , toText
--   )


import qualified Bio.PDB.Type  as PDB
import           Bio.Structure

import           Control.Arrow ((&&&))
import qualified Data.Vector as V (Vector, length, fromList, empty, (!), toList)
import           Data.Foldable (Foldable (..))
import           Data.Text     as T (Text, singleton, unpack)
import           Linear.V3     (V3 (..))



import           Bio.PDB.Parser         (CoordLike (..), 
                                        coordLikeP, manyModelsP, titleP --,otherFieldsP
                                        , remarkP, pdbP)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
-- import           Data.Text              (Text)
import           Data.Text              (pack, null, intercalate, concat, append)
import qualified Data.Text.IO           as TIO (readFile, writeFile)
import           Data.Map.Strict (Map, assocs)

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

-- test :: V.Vector Text -> Text
-- test d = foldr (append) "" (toList d)

-- otherFieldToText :: (PDB.FieldType, PDB.FieldData) -> Text -> Text
-- otherFieldToText (fieldType, fieldData) = append (pack $ "\n" ++ show (test (map (append "\n") fieldData)) ++  show fieldType)


-- otherFieldToText :: (PDB.FieldType, PDB.FieldData) -> Text -> Text
-- otherFieldToText (fieldType, fieldData) = append (pack $ "\n" ++ show (map (append "\n") (toList fieldData)) ++  show fieldType)

pdbToText :: PDB.PDB -> Text
pdbToText PDB.PDB{..} = interNewLine parts <> "\n"
    where
      parts = [ title
              , "MODELS\n"
              -- , models
              , "REMARKS\n"
              , pack $ show (assocs remarks)
              , "\nOTHERFIELDS\n"
              -- , pack $ show (assocs otherFields)
              -- , foldr otherFieldToText "" (assocs otherFields)
              -- , test (head (assocs otherFields))
              ]

toFile :: MonadIO m => PDB.PDB -> FilePath -> m ()
toFile s f = liftIO $ TIO.writeFile f $ pdbToText s


