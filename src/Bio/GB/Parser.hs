{-# LANGUAGE OverloadedStrings #-}

module Bio.GB.Parser
  ( genBankP
  , rangeP
  ) where

import Bio.GB.Type                (Feature (..), Form (..), GenBankSequence (..), Locus (..),
                                   Meta (..), Parser, Reference (..), Source (..), Version (..))
import Bio.Sequence               (Border (..), MarkedSequence, Range (..), RangeBorder (..),
                                   markedSequence)
import Control.Monad.Combinators  (many, manyTill, optional, some, (<|>))
import Data.Char                  (isAlphaNum, isSpace, isUpper)
import Data.Functor               (($>))
import Data.Text                  (Text, intercalate, pack, splitOn, unpack)
import Text.Megaparsec            (option, satisfy, sepBy1, takeWhile1P, takeWhileP, try, (<?>))
import Text.Megaparsec.Char       (char, digitChar, eol, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- | Parser of .gb file.
--
genBankP :: Parser GenBankSequence
genBankP =  GenBankSequence
        <$> (metaP <?> "Meta parser")
        <*> (gbSeqP <?> "GB sequence parser")
        <*  string "//" <* eolSpaceP 

--------------------------------------------------------------------------------
-- Block with meta-information.
--------------------------------------------------------------------------------

metaP :: Parser Meta
metaP = do
  locus'      <- locusP <?> "Locus parser"

  definitionM <- optional definitionP <?> "Definition parser"
  accessionM  <- optional accessionP <?> "Accession parser"
  versionM    <- optional versionP <?> "Version parser"
  keywordsM   <- optional keywordsP <?> "Keywords parser"
  sourceM     <- optional sourceP <?> "Source parser"
  referencesL <- many referenceP <?> "References parser"
  commentsL   <- many commentP <?> "Comments parser"

  pure $ Meta locus' definitionM accessionM versionM keywordsM sourceM referencesL commentsL

locusP :: Parser Locus
locusP = string "LOCUS" *> space *> (Locus
       <$> textP <* space                                      -- name
       <*> decimal <* space <* string "bp" <* space            -- sequence length
       <*> textP <* space                                      -- molecule type
       <*> optional formP <* space                               -- form of sequence
       <*> optional (pack <$> some (satisfy isUpper)) <* space   -- GenBank division
       <*> textP                                               -- modification date
       <*  eolSpaceP)
  where
    textP = takeWhile1P Nothing $ not . isSpace

    formP :: Parser Form
    formP = try (string "linear" $> Linear) <|> (string "circular" $> Circular)

definitionP :: Parser Text
definitionP =  string "DEFINITION" *> space *> (try emptyP <|> someLinesP)

accessionP :: Parser Text
accessionP =  string "ACCESSION" *> space *> (try emptyP <|> (pack
          <$> some (try alphaNumChar <|> char '_')
          <*  eolSpaceP))

versionP :: Parser Version
versionP =  string "VERSION" *> space
         *> ((Version <$> emptyP <*> pure Nothing) <|> (Version
        <$> (pack <$> some versionP')
        <*> optional (pack <$> (space *> string "GI:" *> some versionP'))
        <*  eolSpaceP))
  where
    versionP' = try alphaNumChar <|> try (char '_') <|> char '.'

keywordsP :: Parser Text
keywordsP =  string "KEYWORDS"
          *> (try emptyP
         <|> (space *> textWithSpacesP <* eolSpaceP))

sourceP :: Parser Source
sourceP =  string "SOURCE" *> space
        *> (Source
       <$> someLinesP
       <*> optional organismP)
  where
    organismP = string "  ORGANISM" *> space *> someLinesP

referenceP :: Parser Reference
referenceP = string "REFERENCE" *> space
           *> (((\x -> Reference x Nothing Nothing Nothing Nothing) <$> emptyP) <|> (Reference
          <$> someLinesP
          <*> optional (string "  AUTHORS" *> space *> someLinesP)
          <*> optional (string "  TITLE" *> space *> someLinesP)
          <*> optional (string "  JOURNAL" *> space *> someLinesP)
          <*> optional (string "  PUBMED" *> space *> someLinesP)))

commentP :: Parser Text
commentP = string "COMMENT" *> (try emptyP <|> (many (char ' ') *> someLinesP))

--------------------------------------------------------------------------------
-- Block with FEATURES table.
--------------------------------------------------------------------------------

featuresP :: Parser [(Feature, Range)]
featuresP = -- skip unknown fields and stop on line with "FEATURES" 
          manyTill (textWithSpacesP <* eolSpaceP) (string "FEATURES") *> space
          *> textWithSpacesP <* eolSpaceP
          *> some (featureP <?> "Single feature parser")

featureP :: Parser (Feature, Range)
featureP = do
    _ <- string featureIndent1

    featureName'      <- takeWhileP Nothing (not . isSpace) <* space
    range <- rangeP <* eolSpaceP

    props <- some propsP

    pure (Feature featureName' props, range)

rangeP :: Parser Range
rangeP =  try spanP 
      <|> try betweenP 
      <|> try pointP
      <|> try joinP
      <|> complementP
  where
    spanP :: Parser Range
    spanP = do
        lowerBorderType <- option Precise (try $ char '<' *> pure Exceeded)
        lowerBorderLocation <- decimal
        _ <- string ".."
        upperBorderType <- option Precise (try $ char '>' *> pure Exceeded)
        upperBorderLocation <- decimal
        pure $ Span (RangeBorder lowerBorderType lowerBorderLocation) (RangeBorder upperBorderType upperBorderLocation) 
                
    betweenP :: Parser Range
    betweenP = do
        before <- decimal
        _ <- char '^'
        after <- decimal
        pure $ Between before after

    pointP :: Parser Range
    pointP = fmap Point decimal
   
    joinP :: Parser Range
    joinP = string "join(" *> fmap Join (rangeP `sepBy1` char ',') <* char ')'

    complementP :: Parser Range
    complementP = fmap Complement $ string "complement(" *> rangeP <* char ')'
        

propsP :: Parser (Text, Text)
propsP = do
    _ <- string featureIndent2
    _ <- char '/'
    propName <- takeWhile1P Nothing (/= '=')
    _ <- char '='

    propText <- try ((char '\"' *> takeWhile1P Nothing (/= '\"') <* char '\"')
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
       <*> some (space *> some digitChar *> space1
        *> some (some letterChar <* (try space1 <|> eolSpaceP)))
  where
    toText :: [[String]] -> String
    toText = concat . fmap concat

--------------------------------------------------------------------------------
-- Parser of 'GenBankSequence' from FEATURES and ORIGIN tables.
--------------------------------------------------------------------------------
gbSeqP :: Parser (MarkedSequence Feature Char)
gbSeqP = do
    features <- (featuresP <?> "Features parser")

    -- | An extract from the GB specification (https://www.ncbi.nlm.nih.gov/genbank/release/current/):
    --    NOTE: The BASE COUNT linetype is obsolete and was removed
    --    from the GenBank flatfile format in October 2003.
    --  Anyway, here, in 2021, we still might get plasmids with the BASE COUNT line present.
    --
    _ <- optional $ try (string "BASE COUNT" *> textWithSpacesP *> eol)

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
eolSpaceP = () <$ many (char ' ') <* eol

emptyP :: Parser Text
emptyP = many (char ' ') *> char '.' *> eolSpaceP *> pure "."

textWithSpacesP :: Parser Text
textWithSpacesP = takeWhileP Nothing (`notElem` ['\n', '\r'])

someLinesP :: Parser Text
someLinesP = intercalate "\n" <$> someLinesIndentP firstIndent

someLinesIndentP :: Text -> Parser [Text]
someLinesIndentP indent =  (:) <$> textWithSpacesP <* eolSpaceP
                       <*> (many (string indent *> textWithSpacesP <* eolSpaceP))

space :: Parser ()
space = () <$ (many $ satisfy isSpace)

space1 :: Parser ()
space1 = () <$ (some $ satisfy isSpace)

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum
