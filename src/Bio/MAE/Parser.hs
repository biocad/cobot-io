{-# LANGUAGE TupleSections #-}

module Bio.MAE.Parser
  ( maeP
  , versionP
  , blockP
  , tableP
  ) where

import           Bio.MAE.Type         (Block (..), Mae (..), MaeValue (..),
                                       Table (..))
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
    fieldsP :: Parser (Map Text MaeValue)
    fieldsP = do
        fieldNames  <- upToDelimiterP lineP
        fieldMaeValues <- replicateM (length fieldNames) lineP

        M.fromList <$> zipWithM (\k v -> (k,) <$> textToMaeValue k v) fieldNames fieldMaeValues

textToMaeValue :: Text -> Text -> Parser MaeValue
textToMaeValue k v = if v == absentMaeValue then pure Absent else
    case T.uncons k of
        Just (c, _) -> getMaeValueReader c v
        _           -> fail "Absent field name."
  where
    absentMaeValue :: Text
    absentMaeValue = "<>"

    getMaeValueReader :: Char -> Text -> Parser MaeValue
    getMaeValueReader 'i' = textToIntMaeValueReader
    getMaeValueReader 'r' = textToRealMaeValueReader
    getMaeValueReader 'b' = textToBoolMaeValueReader
    getMaeValueReader 's' = textToStringMaeValueReader
    getMaeValueReader _   = const $ fail "Unknown value type."

    textToIntMaeValueReader :: Text -> Parser MaeValue
    textToIntMaeValueReader = either fail (pure . IntMaeValue . fst) . TR.signed TR.decimal

    textToRealMaeValueReader :: Text -> Parser MaeValue
    textToRealMaeValueReader = either fail (pure . RealMaeValue . fst) . TR.signed TR.rational

    textToBoolMaeValueReader :: Text -> Parser MaeValue
    textToBoolMaeValueReader t =
        case t of
            "0" -> pure $ BoolMaeValue False
            "1" -> pure $ BoolMaeValue True
            _   -> fail "Can't parse bool value."

    textToStringMaeValueReader :: Text -> Parser MaeValue
    textToStringMaeValueReader = pure . StringMaeValue

tableP :: Parser Table
tableP = do
    name            <- many' oneSpaceP *> takeWhile1 (/= leftSquareBracket)
    numberOfEntries <- char leftSquareBracket *> decimal <* char rightSquareBracket

    _ <- many' oneSpaceP

    contents <- inBrackets $ do
        fieldNames  <- upToDelimiterP lineP
        let readers = fmap textToMaeValue fieldNames
        entries     <- replicateM numberOfEntries $ entryP readers

        delimiterP

        pure $ M.fromList $ zip fieldNames $ transpose entries

    pure $ Table name contents
  where
    leftSquareBracket :: Char
    leftSquareBracket = '['

    rightSquareBracket :: Char
    rightSquareBracket = ']'

    entryP :: [Text -> Parser MaeValue] -> Parser [MaeValue]
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
