module Bio.GB.Functions where

import Debug.Trace
import Data.Attoparsec.Text
import Bio.GB.Parser
import Bio.GB.Writer
import Bio.Sequence
import Bio.FASTA.Parser
import qualified Bio.FASTA.Type as FASTA
import Bio.GB.Type
import Data.Text.IO as TIO
import Data.Text as T
import Data.Vector as V
import Data.List as L
import Control.Lens
import Data.Maybe
import Control.Monad

changeInsertion :: IO ()
changeInsertion = do
    gb <- readAndParse "/Users/agromova/tmp/BCD216.gb"
    changes' <- changes "/Users/agromova/tmp/fasta.fasta"
    Control.Monad.forM_ changes' $ \(newName, newSeq) -> do
        let res = updateGB "BCD216-00_ABVH_000_01" newName newSeq gb
        let newFile = genName newName
        let newGB   = genBankToText res
        TIO.writeFile newFile newGB
  where
    genName :: Text -> FilePath
    genName t = "/Users/agromova/tmp/plasmid_maps/pSXn-IgG4HCpos-NR_" <> T.unpack t <> ".gb"



readAndParse :: FilePath -> IO GenBankSequence
readAndParse path = do
    content <- TIO.readFile path
    case parseOnly genBankP content of
        Left e -> error e
        Right r -> pure r

updateGB :: Name -> Name -> Text -> GenBankSequence -> GenBankSequence
updateGB oldFeatureName newFeatureName newSeq GenBankSequence{..} = GenBankSequence meta newGbSeq
  where
    newSeqVector = V.fromList . T.unpack $ newSeq

    newGbSeq = trace (show newFullSeq) $ trace (show updatedFeature) unsafeMarkedSequence (V.toList newFullSeq) (updatedFeature:movedFeatures) 
    
    oldMarkings = gbSeq ^. markings
    (featuresToUpdate, featuresToMove) = L.partition (needFeatureUpdate oldFeatureName) oldMarkings

    (oldFeature, location) = case featuresToUpdate of
                                 [(f, l)] -> (f, l)
                                 _ -> error "Too many features to update"
    
    (newFullSeq, offset, delta) = updateSequence (gbSeq ^. sequ) location newSeqVector
    
    updatedFeature = updateFeature (oldFeature, location)
                                   oldFeatureName
                                   newFeatureName
                                   newSeqVector
    movedFeatures = moveMarking (offset, delta) <$> featuresToMove 

-- | If feature has the same name as feature that we need to update
--
needFeatureUpdate :: Name -> (Feature, Range) -> Bool
needFeatureUpdate nameToUpdate (Feature {..}, _) 
    = isJust . L.find ((== nameToUpdate) . snd) $ fProps

updateFeature :: (Feature, Range) -- feature with range
              -> Name             -- name of the feature to update
              -> Name             -- name of the new feature
              -> Vector Char      -- sequence of the new feature
              -> (Feature, Range) 
updateFeature (Feature {..}, (start, _)) oldName newName newSeq 
    = (Feature fName fStrand53 newProps, (start, start + V.length newSeq))
  where
    newProps = [ if v == oldName then (k, newName) else (k, v)
               | (k, v) <- fProps
               ]

type Offset = Int
type Delta  = Int

updateSequence :: Vector a -> (Int, Int) -> Vector a -> (Vector a, Offset, Delta)
updateSequence initial (start, end) partToChange = (updatedVector, start, delta)
  where
    before = V.take start initial
    after  = V.drop end   initial 
    updatedVector = before <> partToChange <> after

    delta = V.length partToChange - (end - start)
    
moveMarking :: (Offset, Delta) -> (marking, Range) -> (marking, Range)
moveMarking (offset, delta) (mk, (start, end))
    | start < offset && end <= offset = (mk, (start, end))
    | offset <= start && offset < end = (mk, (start + delta, end + delta))
    | otherwise = error "updateMarking: offset is in the marking"

changes :: FilePath -> IO [(Name, Text)]
changes file = do
    content <- TIO.readFile file
    let Right fasta = parseOnly fastaP content 
    pure $ L.map (\fi -> (FASTA.name fi, FASTA.sequ fi ^. sequ . to V.toList . to T.pack)) fasta
