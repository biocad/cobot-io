{-# LANGUAGE OverloadedStrings #-}

module Bio.GB.Parser
  ( genBankP
  ) where

import Bio.GB.Type                (Feature (..), Form (..), GenBankSequence (..), Locus (..),
                                   Meta (..), Reference (..), Source (..), Version (..))
import Bio.Sequence               (MarkedSequence, Range, markedSequence)
import Control.Applicative        ((<|>))
import Data.Attoparsec.Combinator (manyTill)
import Data.Attoparsec.Text       (Parser, char, decimal, digit, endOfInput, endOfLine, letter,
                                   many', many1', satisfy, string, takeWhile, takeWhile1, (<?>))
import Data.Bifunctor             (bimap)
import Data.Char                  (isAlphaNum, isSpace, isUpper)
import Data.Functor               (($>))
import Data.Text                  (Text, intercalate, pack, splitOn, unpack)
import Prelude                    hiding (takeWhile)

-- | Parser of .gb file.
--
genBankP :: Parser GenBankSequence
genBankP =  GenBankSequence
        <$> (metaP <?> "Meta parser")
        <*> (gbSeqP <?> "GB sequence parser")
        <*  string "//" <* eolSpaceP <* endOfInput

--------------------------------------------------------------------------------
-- Block with meta-information.
--------------------------------------------------------------------------------

metaP :: Parser Meta
metaP = do
  locus'      <- locusP <?> "Locus parser"

  definitionM <- wrapMP definitionP <?> "Definition parser"
  accessionM  <- wrapMP accessionP <?> "Accession parser"
  versionM    <- wrapMP versionP <?> "Version parser"
  keywordsM   <- wrapMP keywordsP <?> "Keywords parser"
  sourceM     <- wrapMP sourceP <?> "Source parser"
  referencesL <- many' referenceP <?> "References parser"
  commentsL   <- many' commentP <?> "Comments parser"

  pure $ Meta locus' definitionM accessionM versionM keywordsM sourceM referencesL commentsL

locusP :: Parser Locus
locusP = string "LOCUS" *> space *> (Locus
       <$> textP <* space                                      -- name
       <*> decimal <* space <* string "bp" <* space            -- sequence length
       <*> textP <* space                                      -- molecule type
       <*> wrapMP formP <* space                               -- form of sequence
       <*> wrapMP (pack <$> many1' (satisfy isUpper)) <* space -- GenBank division
       <*> textP                                               -- modification date
       <*  eolSpaceP)
  where
    textP = takeWhile1 $ not . isSpace

    formP :: Parser Form
    formP = (string "linear" $> Linear) <|> (string "circular" $> Circular)

definitionP :: Parser Text
definitionP =  string "DEFINITION" *> space *> (emptyP <|> someLinesP)

accessionP :: Parser Text
accessionP =  string "ACCESSION" *> space *> (emptyP <|> (pack
          <$> many1' (alphaNumChar <|> char '_')
          <*  eolSpaceP))

versionP :: Parser Version
versionP =  string "VERSION" *> space
         *> ((Version <$> emptyP <*> pure Nothing) <|> (Version
        <$> (pack <$> many1' versionP')
        <*> wrapMP (pack <$> (space *> string "GI:" *> many1' versionP'))
        <*  eolSpaceP))
  where
    versionP' = alphaNumChar <|> char '_' <|> char '.'

keywordsP :: Parser Text
keywordsP =  string "KEYWORDS"
          *> (emptyP
         <|> (space *> textWithSpacesP <* eolSpaceP))

sourceP :: Parser Source
sourceP =  string "SOURCE" *> space
        *> (Source
       <$> someLinesP
       <*> wrapMP organismP)
  where
    organismP = string "  ORGANISM" *> space *> someLinesP

referenceP :: Parser Reference
referenceP = string "REFERENCE" *> space
           *> (((\x -> Reference x Nothing Nothing Nothing Nothing) <$> emptyP) <|> (Reference
          <$> someLinesP
          <*> wrapMP (string "  AUTHORS" *> space *> someLinesP)
          <*> wrapMP (string "  TITLE" *> space *> someLinesP)
          <*> wrapMP (string "  JOURNAL" *> space *> someLinesP)
          <*> wrapMP (string "  PUBMED" *> space *> someLinesP)))

commentP :: Parser Text
commentP = string "COMMENT" *> (emptyP <|> (many' (char ' ') *> someLinesP))

--------------------------------------------------------------------------------
-- Block with FEATURES table.
--------------------------------------------------------------------------------

featuresP :: Parser [(Feature, Range)]
featuresP = -- skip unknown fields and stop on line with "FEATURES" 
          manyTill (textWithSpacesP <* eolSpaceP) (string "FEATURES") *> space
          *> textWithSpacesP <* eolSpaceP
          *> many1' (featureP <?> "Single feature parser")

featureP :: Parser (Feature, Range)
featureP = do
    _ <- string featureIndent1

    featureName'      <- takeWhile (not . isSpace) <* space
    (strand53, range) <- rangeP <* eolSpaceP

    props <- many1' propsP

    pure (Feature featureName' strand53 props, range)

rangeP :: Parser (Bool, Range)
rangeP =  (string "join" *> fail "Unsupported range with join(..)")
      <|> (string "complement(" *> rP False <* char ')') 
      <|> rP True
  where
    rP :: Bool -> Parser (Bool, Range)
    rP b =  fmap (bimap pred id)
        <$> (,) b
        <$> (((,) <$> decimal <* string ".." <*> decimal) <|> ((\x -> (x, x)) <$> decimal))

propsP :: Parser (Text, Text)
propsP = do
    _ <- string featureIndent2
    _ <- char '/'
    propName <- takeWhile1 (/= '=')
    _ <- char '='

    propText <- ((char '\"' *> takeWhile1 (/= '\"') <* char '\"')
             <|> textWithSpacesP)
             <* eolSpaceP

    let propTextCorrect = mconcat $ filter (/= featureIndent2) $ splitOn featureIndent2 propText

    pure (propName, propTextCorrect)

-- | First level of identation in FEATURES table file.
--
featureIndent1 :: Text
featureIndent1 = pack $ replicate 5 ' '

-- | Second level of identation in FEATURES table file.
--
featureIndent2 :: Text
featureIndent2 = pack $ replicate 21 ' '

--------------------------------------------------------------------------------
-- Block with ORIGIN table.
--------------------------------------------------------------------------------

originP :: Parser String
originP =  (string "ORIGIN" <?> "String ORIGIN") *> eolSpaceP
        *> pure toText
       <*> many1' (space *> many1' digit *> space1
        *> many1' (many1' letter <* (space1 <|> eolSpaceP)))
  where
    toText :: [[String]] -> String
    toText = concat . fmap concat

--------------------------------------------------------------------------------
-- Parser of 'GenBankSequence' from FEATURES and ORIGIN tables.
--------------------------------------------------------------------------------
gbSeqP :: Parser (MarkedSequence Feature Char)
gbSeqP = do
    features <- (featuresP <?> "Features parser")
    origin   <- (originP <?> "Origin parser")

    either (fail . unpack) pure (markedSequence origin features)

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

-- | First level of identation in .gb file.
--
firstIndent :: Text
firstIndent = pack $ replicate 12 ' '

eolSpaceP :: Parser ()
eolSpaceP = () <$ many' (char ' ') <* endOfLine

emptyP :: Parser Text
emptyP = many' (char ' ') *> char '.' *> eolSpaceP *> pure "."

textWithSpacesP :: Parser Text
textWithSpacesP = takeWhile (`notElem` ['\n', '\r'])

someLinesP :: Parser Text
someLinesP = intercalate "\n" <$> someLinesIndentP firstIndent

someLinesIndentP :: Text -> Parser [Text]
someLinesIndentP indent =  (:) <$> textWithSpacesP <* eolSpaceP
                       <*> (many' (string indent *> textWithSpacesP <* eolSpaceP))

wrapMP :: Parser a -> Parser (Maybe a)
wrapMP p = fmap Just p <|> pure Nothing

space :: Parser ()
space = () <$ (many' $ satisfy isSpace)

space1 :: Parser ()
space1 = () <$ (many1' $ satisfy isSpace)

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum
