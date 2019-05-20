module Bio.GB.Writer
  ( genBankToText
  ) where

import           Bio.GB.Type     (Feature (..), GenBankSequence (..),
                                  Locus (..), Meta (..), Reference (..),
                                  Source (..), Version (..))
import           Bio.Sequence    (Range, markings, toList)
import           Control.Lens    ((^.))
import qualified Data.List.Split as S (chunksOf)
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T (append, chunksOf, intercalate, length,
                                       lines, null, pack, toLower, unwords)

genBankToText :: GenBankSequence -> Text
genBankToText GenBankSequence{..} = interNewLine parts <> "\n"
  where
    parts = [ metaToText meta
            , featuresToText $ gbSeq ^. markings
            , originToText $ T.pack $ toList gbSeq
            , "//"
            ]

--------------------------------------------------------------------------------
-- Block with meta-information.
--------------------------------------------------------------------------------

metaToText :: Meta -> Text
metaToText Meta{..} = interNewLine parts
  where
    parts = [ locusToText locus
            , textFromMaybe $ fmap (processMany metaIndent "DEFINITION") definition
            , textFromMaybe $ fmap (processMany metaIndent "ACCESSION") accession
            , textFromMaybe $ fmap versionToText version
            , textFromMaybe $ fmap (processMany metaIndent "KEYWORDS") keywords
            , textFromMaybe $ fmap sourceToText source
            ]
            <> fmap referenceToText references
            <> fmap (processMany metaIndent "COMMENT") comments

locusToText :: Locus -> Text
locusToText Locus{..} = toIndent metaIndent "LOCUS" <> locusText
  where
    locusList = [ name
                , showText len <> " bp"
                , molType
                , textFromMaybe $ fmap (T.toLower . showText) form
                , textFromMaybe gbDivision
                , modificationDate
                ]

    locusText = T.intercalate (spaces 5) $ filter (not . T.null) locusList

sourceToText :: Source -> Text
sourceToText Source{..} = interNewLine $ mainPart : pure (textFromMaybe organismPart)
  where
    mainPart     = processMany metaIndent "SOURCE" sourceT
    organismPart = fmap (processMany metaIndent (prependIndent metaPreIndent "ORGANISM")) organism

versionToText :: Version -> Text
versionToText Version{..} = toIndent metaIndent "VERSION" <> version
   where
     version = versionT <> spaces 5 <> maybe mempty ("GI:" <>) gbId

referenceToText :: Reference -> Text
referenceToText Reference{..} = interNewLine $ mainPart : parts
  where
    mainPart = processMany metaIndent "REFERENCE" referenceT

    sectionNames = fmap (prependIndent metaPreIndent) ["AUTHORS", "TITLE", "JOURNAL", "PUBMED"]
    sections     = [authors, title, journal, pubmed]

    parts = zipWith (\a -> textFromMaybe . fmap (processMany metaIndent a)) sectionNames sections

-- | Indentation of data in section with metainformation.
--
metaIndent :: Int
metaIndent = 12

-- | Indentation in subsections of section with meta-information.
--
metaPreIndent :: Int
metaPreIndent = 2

featuresToText :: [(Feature, Range)] -> Text
featuresToText l = interNewLine $ mainPart : sections
  where
    mainPart = processMany featuresIndent "FEATURES" featuresText
    sections = fmap featureToText l

    featuresText :: Text
    featuresText = "Location/Qualifiers"

--------------------------------------------------------------------------------
-- Block with FEATURES section.
--------------------------------------------------------------------------------

featureToText :: (Feature, Range) -> Text
featureToText (Feature{..}, range) = interNewLine $ mainPart : sections
  where
    mainPart = processMany featuresIndent (prependIndent featuresPreIndent fName) (featureRangeToText fStrand53 range)
    sections = fmap featurePropToText fProps

featurePropToText :: (Text, Text) -> Text
featurePropToText (nameF, textF) = mainPart
  where
    mainPart = processMany featuresIndent mempty ("/" <> nameF <> "=\"" <> textF <> "\"")

featureRangeToText :: Bool -> Range -> Text
featureRangeToText complement (l, r) | l == r - 1 = processComplement complement $ showText (l + 1)
                                     | otherwise  = processComplement complement $ showText (l + 1) <> ".." <> showText r
  where
    processComplement :: Bool -> Text -> Text
    processComplement True  text = text
    processComplement False text = "complement(" <> text <> ")"

-- | Indentation of feature's properties in FEATURES section.
--
featuresIndent :: Int
featuresIndent = 21

-- | Indentation in subsections of FEATURES section.
--
featuresPreIndent :: Int
featuresPreIndent = 5

--------------------------------------------------------------------------------
-- Block with ORIGIN table.
--------------------------------------------------------------------------------

originToText :: Text -> Text
originToText text = interNewLine $ mainPart : parts
  where
    mainPart = "ORIGIN"

    manyLines = S.chunksOf lengthOfLineChunk $ T.chunksOf lengthOfChunk text
    parts     = zipWith processLine [1, 1 + lengthOfChunk * lengthOfLineChunk..] manyLines

    processLine :: Int -> [Text] -> Text
    processLine startInd = T.unwords . (prependIndent (originIndent - T.length indText) indText :)
      where
        indText = showText startInd

    -- | Number of nucleotides in one chunk.
    --
    lengthOfChunk :: Int
    lengthOfChunk = 10

    -- | Number of chunks per line of sequence in ORIGIN section.
    --
    lengthOfLineChunk :: Int
    lengthOfLineChunk = 6

-- | Indentation of new line of sequence in ORIGIN section.
--
originIndent :: Int
originIndent = 9

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

processMany :: Int -> Text -> Text -> Text
processMany indent name ""   = toIndent indent name
processMany indent name text = interNewLine resLines
  where
    (x : xs) = T.lines text

    resLines = toIndent indent name <> x
             : fmap (prependIndent indent) xs

interNewLine :: [Text] -> Text
interNewLine = T.intercalate "\n" . filter (not . T.null)

textFromMaybe :: Maybe Text -> Text
textFromMaybe = fromMaybe mempty

toIndent :: Int -> Text -> Text
toIndent indent name = name <> (spaces $ indent - (T.length name))

prependIndent :: Int -> Text -> Text
prependIndent = T.append . spaces

showText :: Show a => a -> Text
showText = T.pack . show

spaces :: Int -> Text
spaces = T.pack . flip replicate ' '
