module Bio.GB.PlasmidDesigner (updateGB) where

import Debug.Trace (trace)
import Bio.Sequence (Range, unsafeMarkedSequence, markings, sequ)
import qualified Bio.FASTA.Type as FASTA (FastaItem(..))
import Bio.GB.Type (Feature(..), Name, GenBankSequence(..), PlasmidFormat(..))
import Data.Vector as V (Vector, toList, length, drop, take)
import Data.List as L (find, partition)
import Control.Lens ((^.))
import Data.Maybe (isJust)
import Prelude hiding (sequence)


updateGB :: PlasmidFormat -> FASTA.FastaItem Char -> GenBankSequence
updateGB (PlasmidFormat plasmid stuffer) (FASTA.FastaItem name sequence) = GenBankSequence (meta plasmid) newGbSeq
  where
    newSeqVector = sequence ^. sequ
    newGbSeq = trace (show newFullSeq) $ trace (show updatedFeature) unsafeMarkedSequence (V.toList newFullSeq) (updatedFeature:movedFeatures)

    oldMarkings = (gbSeq plasmid) ^. markings
    (featuresToUpdate, featuresToMove) = L.partition (needFeatureUpdate stuffer) oldMarkings

    (oldFeature, location) = case featuresToUpdate of
                                 [(f, l)] -> (f, l)
                                 _ -> error "Too many features to update"

    (newFullSeq, offset, delta) = updateSequence ((gbSeq plasmid) ^. sequ) location newSeqVector

    updatedFeature = updateFeature (oldFeature, location)
                                   stuffer
                                   name
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
