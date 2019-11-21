{-# LANGUAGE TupleSections #-}

module Bio.MAE.Parser where

import           Bio.MAE.Type         (Block (..), Mae (..), Table (..),
                                       Value (..))
import           Control.Applicative  ((<|>))
import           Control.Monad        (replicateM, when, zipWithM)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfInput,
                                       endOfLine, many', many1', string,
                                       takeWhile, takeWhile1)
import           Data.Char            (isSpace)
import           Data.List            (transpose)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M (fromList)
import           Data.Text            (Text)
import qualified Data.Text            as T (pack, uncons)
import qualified Data.Text.Read       as TR (decimal, rational, signed)
import           Prelude              hiding (takeWhile)

maeP :: Parser Mae
maeP = Mae <$> versionP
           <*> many' blockP
           <*  endOfInput

versionP :: Parser Text
versionP = inBrackets $  lineP
                      *> delimiterP
                      *> lineP

blockP :: Parser Block
blockP = uncurry <$> (Block <$> anyStringP <* many' oneSpaceP)
                 <*> inBrackets ((,) <$> fieldsP <*> many' tableP)
  where
    fieldsP :: Parser (Map Text Value)
    fieldsP = do
        fieldNames  <- upToDelimiterP lineP
        fieldValues <- replicateM (length fieldNames) lineP

        M.fromList <$> zipWithM (\k v -> (k,) <$> textToValue k v) fieldNames fieldValues

textToValue :: Text -> Text -> Parser Value
textToValue k v = if v == absentValue then pure Absent else
    case T.uncons k of
        Just (c, _) -> getValueReader c v
        _           -> fail "Absent field name."
  where
    absentValue :: Text
    absentValue = "<>"

    getValueReader :: Char -> Text -> Parser Value
    getValueReader 'i' = textToIntValueReader
    getValueReader 'r' = textToRealValueReader
    getValueReader 'b' = textToBoolValueReader
    getValueReader 's' = textToStringValueReader
    getValueReader _   = const $ fail "Unknown value type."

    textToIntValueReader :: Text -> Parser Value
    textToIntValueReader = either fail (pure . IntValue . fst) . TR.signed TR.decimal

    textToRealValueReader :: Text -> Parser Value
    textToRealValueReader = either fail (pure . RealValue . fst) . TR.signed TR.rational

    textToBoolValueReader :: Text -> Parser Value
    textToBoolValueReader t =
        case t of
            "0" -> pure $ BoolValue False
            "1" -> pure $ BoolValue True
            _   -> fail "Can't parse bool value."

    textToStringValueReader :: Text -> Parser Value
    textToStringValueReader = pure . StringValue

tableP :: Parser Table
tableP = do
    name            <- many' oneSpaceP *> takeWhile1 (/= leftSquareBracket)
    numberOfEntries <- char leftSquareBracket *> decimal <* char rightSquareBracket

    _ <- many' oneSpaceP

    contents <- inBrackets $ do
        fieldNames  <- upToDelimiterP lineP
        let readers = fmap textToValue fieldNames
        entries     <- replicateM numberOfEntries $ entryP readers

        delimiterP

        pure $ M.fromList $ zip fieldNames $ transpose entries

    pure $ Table name contents
  where
    leftSquareBracket :: Char
    leftSquareBracket = '['

    rightSquareBracket :: Char
    rightSquareBracket = ']'

    entryP :: [Text -> Parser Value] -> Parser [Value]
    entryP readers = do
        valuesT <- many1' (many' oneSpaceP *> valueTP <* many' oneSpaceP) <* tillEndOfLine
        when (length readers /= length valuesT - 1) $ fail "Wrong number of values in an entry."
        zipWithM ($) readers $ drop 1 valuesT

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

inBrackets :: Parser a -> Parser a
inBrackets p =  char leftBracket *> many1' tillEndOfLine
             *> p
             <* many' oneSpaceP <* char rightBracket <* many1' tillEndOfLine
  where
    leftBracket :: Char
    leftBracket = '{'

    rightBracket :: Char
    rightBracket = '}'

delimiterP :: Parser ()
delimiterP = many' oneSpaceP *> string delimiter *> tillEndOfLine
  where
    delimiter :: Text
    delimiter = ":::"

upToDelimiterP :: Parser a -> Parser [a]
upToDelimiterP p = ([] <$ delimiterP) <|> ((:) <$> p <*> upToDelimiterP p)

oneSpaceP :: Parser Char
oneSpaceP = char ' '

anyStringP :: Parser Text
anyStringP = takeWhile1 (not . isSpace)

valueTP :: Parser Text
valueTP  =  ((<>) <$> string quoteT <*> ((<>) <$> takeWhile (/= quote) <*> string quoteT))
        <|> anyStringP
  where
    quote :: Char
    quote = '\"'

    quoteT :: Text
    quoteT = T.pack $ pure quote

commentaryP :: Parser ()
commentaryP = () <$ many' (many' oneSpaceP *> char '#' *> takeWhile (`notElem` ['\n', '\r']) *> endOfLine)

lineP :: Parser Text
lineP = commentaryP *> many' oneSpaceP *> valueTP <* tillEndOfLine <* commentaryP

tillEndOfLine :: Parser ()
tillEndOfLine = () <$ many' oneSpaceP <* endOfLine
