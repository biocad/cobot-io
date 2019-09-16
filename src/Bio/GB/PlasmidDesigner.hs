module Bio.GB.PlasmidDesigner (updateGB) where

import Bio.Sequence (MarkedSequence, Range, Element, unsafeMarkedSequence, markings, sequ)
import Bio.FASTA.Type hiding (sequ)
import Bio.GB.Type (Feature(..), GenBankSequence(..), PlasmidFormat(..))
import Data.Vector as V (Vector, toList, length, drop, take)
import Data.List (tail, find, span)
import Control.Lens ((^.))
import Prelude hiding (sequence)
import Data.Text (Text, unpack)

data ReplacementError = NoSuchElement String

instance Show ReplacementError where
  show (NoSuchElement str) = str

updateGB :: PlasmidFormat -> FastaItem Char -> GenBankSequence
updateGB (PlasmidFormat (GenBankSequence meta plasmidSeq) stuffer) fasta = GenBankSequence meta gbSeq
    where
        stufferFeature = getStufferFeature (plasmidSeq ^. markings) stuffer
        sequence = updateSequence (plasmidSeq ^. sequ) stufferFeature fasta
        features = updateFeatures (plasmidSeq ^. markings) stufferFeature fasta

        gbSeq = unsafeMarkedSequence sequence features

getStufferFeature :: [(Feature, Range)] -> Text -> (Feature, Range)
getStufferFeature features elementName =
    case find (\(feature, _) -> hasName feature elementName) features of
            Nothing -> error ("There is no element with name " <> (unpack elementName) <> " in plasmid")
            Just v -> v

hasName :: Feature -> Text -> Bool
hasName feature elementName = case lookup "label" (fProps feature) of
                                   Nothing -> False
                                   Just v -> v == elementName

updateSequence :: Vector Char -> (Feature, Range) -> FastaItem Char -> [Element (MarkedSequence Feature Char)]
updateSequence initialSeq (_, (start, end)) (FastaItem _ sequence) =  V.toList (before <> fastaSequ <> after)
    where
        fastaSequ = sequence ^. sequ
        before = V.take start initialSeq
        after  = V.drop end   initialSeq

updateFeatures :: [(Feature, Range)] -> (Feature, Range) -> FastaItem Char -> [(Feature, Range)]
updateFeatures features stuffer@(_, (stufferStart, stufferEnd)) (FastaItem name sequence) =  [new] ++ old ++ changed
    where
        sortedFeatures = span (\(_, (start, _)) -> start < stufferStart) features
        delta = (V.length $ sequence ^. sequ) - (stufferEnd - stufferStart)

        old = fst sortedFeatures
        new = changeFeature stuffer delta name
        changed = shiftRanges delta (tail $ snd sortedFeatures)

changeFeature :: (Feature, Range) -> Int -> Text -> (Feature, Range)
changeFeature (Feature {..}, (start, end)) delta name = (Feature fName fStrand53 newProps, (start, end + delta))
    where
        newProps = [ if elem k ["locus_tag", "label", "ApEinfo_label"] then (k, name) else (k, v) | (k, v) <- fProps]

shiftRanges :: Int -> [(Feature, Range)] -> [(Feature, Range)]
shiftRanges delta = map (\(feature, (start, end)) -> (feature, (start + delta, end + delta)))