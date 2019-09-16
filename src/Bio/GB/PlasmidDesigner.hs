{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bio.GB.PlasmidDesigner (updateGB, DesignerError(..)) where

import           Bio.FASTA.Type       hiding (sequ)
import           Bio.GB.Type          (Feature (..), GenBankSequence (..),
                                       PlasmidFormat (..))
import           Bio.Sequence         (Element, MarkedSequence, Range, markings,
                                       sequ, unsafeMarkedSequence)
import           Control.Lens         ((^.))
import           Control.Monad.Except (MonadError, throwError)
import           Data.List            (find, span, tail)
import           Data.Text            (Text, unpack)
import           Data.Vector          as V (Vector, drop, length, take, toList)
import           GHC.Generics         (Generic)
import           Prelude              hiding (sequence)

data DesignerError
    = WrongArgumentsFormat String
    | NoSuchElement String
  deriving (Eq, Generic, Show)

updateGB :: (MonadError DesignerError m) => PlasmidFormat -> FastaItem Char -> m GenBankSequence
updateGB (PlasmidFormat GenBankSequence{..} stuffer) fasta@(FastaItem _ s)
    | null s = throwError $ WrongArgumentsFormat "Empty fasta sequence"
    | otherwise = do
        stufferFeature <- getStufferFeature (gbSeq ^. markings) stuffer
        let sequence = updateSequence (gbSeq ^. sequ) stufferFeature fasta
        let features = updateFeatures (gbSeq ^. markings) stufferFeature fasta

        let newGbSeq = unsafeMarkedSequence sequence features
        pure $ GenBankSequence meta newGbSeq

getStufferFeature :: (MonadError DesignerError m) => [(Feature, Range)] -> Text -> m (Feature, Range)
getStufferFeature [] _ = throwError $ WrongArgumentsFormat "Could not find any features in plasmid map"
getStufferFeature features elementName =
    case find (\(feature, _) -> hasName feature) features of
            Nothing -> throwError $ NoSuchElement ("There is no element with name " <> (unpack elementName) <> " in plasmid")
            Just v -> pure v
    where
        hasName :: Feature -> Bool
        hasName feature = case lookup "label" (fProps feature) of
                                           Nothing -> False
                                           Just v  -> v == elementName

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
