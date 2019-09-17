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
import           Data.List            (filter, find, partition, tail)
import           Data.Text            (Text, unpack)
import           Data.Vector          as V (Vector, drop, length, take, toList)
import           GHC.Generics         (Generic)

data DesignerError
    = WrongArgumentsFormat String
    | NoSuchElement String
  deriving (Eq, Generic, Show)

updateGB :: (MonadError DesignerError m) => PlasmidFormat -> FastaItem Char -> m GenBankSequence
updateGB (PlasmidFormat (GenBankSequence meta gbSeq) stuffer) replacement@(FastaItem _ s)
    | null s = throwError $ WrongArgumentsFormat "Empty fasta sequence"
    | otherwise = do
        stufferFeature <- getStufferFeature (gbSeq ^. markings) stuffer
        let sequence' = updateSequence (gbSeq ^. sequ) stufferFeature replacement
        let features = updateFeatures (gbSeq ^. markings) stufferFeature replacement

        let newGbSeq = unsafeMarkedSequence sequence' features
        pure $ GenBankSequence meta newGbSeq

getStufferFeature :: (MonadError DesignerError m) => [(Feature, Range)] -> Text -> m (Feature, Range)
getStufferFeature [] _ = throwError $ WrongArgumentsFormat "Could not find any features in plasmid map"
getStufferFeature features elementName =
    case filter (\(feature, _) -> hasName feature) features of
            []       -> throwError $ NoSuchElement ("There is no element with name " <> (unpack elementName) <> " in plasmid")
            [v]      -> pure v
            _        -> throwError $ NoSuchElement ("There are several elements with name " <> (unpack elementName) <> " in plasmid")
    where
        hasName :: Feature -> Bool
        hasName feature = case lookup "label" (fProps feature) of
                                           Nothing -> False
                                           Just v  -> v == elementName

updateSequence :: Vector Char -> (Feature, Range) -> FastaItem Char -> [Element (MarkedSequence Feature Char)]
updateSequence initialSeq (_, (stufferStart, stufferEnd)) (FastaItem _ sequence) =  V.toList (before <> fastaSequ <> after)
    where
        fastaSequ = sequence ^. sequ
        before = V.take stufferStart initialSeq
        after  = V.drop stufferEnd   initialSeq

updateFeatures :: [(Feature, Range)] -> (Feature, Range) -> FastaItem Char -> [(Feature, Range)]
updateFeatures features stuffer@(_, (stufferStart, stufferEnd)) (FastaItem name sequence) =  [replacement] ++ before ++ after
    where
        sortedFeatures = partition isBeforeStuffer features
        delta = (V.length $ sequence ^. sequ) - (stufferEnd - stufferStart)

        isBeforeStuffer :: (Feature, Range) -> Bool
        isBeforeStuffer (_, (start, _)) = start < stufferStart

        changeFeature :: (Feature, Range) -> (Feature, Range)
        changeFeature (Feature {..}, (start, end)) = (Feature fName fStrand53 newProps, (start, end + delta))
            where
                newProps = [if elem k ["locus_tag", "label", "ApEinfo_label"] then (k, name) else (k, v) | (k, v) <- fProps]

        before = fst sortedFeatures
        replacement = changeFeature stuffer
        after = shiftRanges delta (filter (/= stuffer) (snd sortedFeatures))

shiftRanges :: Int -> [(Feature, Range)] -> [(Feature, Range)]
shiftRanges delta = map (\(feature, (start, end)) -> (feature, (start + delta, end + delta)))
