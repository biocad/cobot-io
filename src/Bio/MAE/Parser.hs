module Bio.MAE.Parser where

import           Bio.MAE.Type         (Block (..), Mae (..), Table (..),
                                       Value (..), fromValue, toValue)
import           Control.Applicative  ((<|>))
import           Control.Monad        (replicateM, zipWithM)
import           Data.Attoparsec.Text (Parser, char, decimal, digit, endOfInput,
                                       endOfLine, letter, many', many1',
                                       satisfy, string, takeWhile, takeWhile1)
import           Data.Bifunctor       (bimap, first)
import           Data.Char            (isAlphaNum, isSpace, isUpper)
import           Data.Functor         (($>))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M (fromList)
import           Data.Text            as T (Text, intercalate, pack, splitOn,
                                            uncons, unpack)
import           Data.Text.Read       as TR (decimal, rational)
import           Prelude              hiding (takeWhile)

import           Debug.Trace          (traceShow)

maeP :: Parser Mae
maeP = Mae <$> versionP
           <*> many' blockP

versionP :: Parser Text
versionP = inBrackets $  parseLine
                      *> delimiterP
                      *> parseLine

blockP :: Parser Block
blockP = uncurry <$> (Block <$> anyStringP <* many' oneSpaceP)
                 <*> inBrackets ((,) <$> fieldsP <*> many' tableP)
  where
    fieldsP :: Parser (Map Text Value)
    fieldsP = do
        fieldNames  <- upToDelimiterP parseLine
        fieldValues <- replicateM (length fieldNames) parseLine

        M.fromList <$> zipWithM textToValue fieldNames fieldValues

    textToValue :: Text -> Text -> Parser (Text, Value)
    textToValue k v = either fail (pure . (,) k) $
        case T.uncons k of
            Just ('i', _) -> IntValue . fst  <$> TR.decimal v
            Just ('r', _) -> RealValue . fst <$> TR.rational v
            Just ('s', _) -> StringValue     <$> Right v
            _             -> Left "Unknown value type."

tableP :: Parser Table
tableP = pure $ Table mempty mempty

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

inBrackets :: Parser a -> Parser a
inBrackets p =  string leftBracket *> manyTillEndOfLine1
             *> p
             <* string rightBracket <* manyTillEndOfLine1

leftBracket :: Text
leftBracket = "{"

rightBracket :: Text
rightBracket = "}"

delimiter :: Text
delimiter = ":::"

delimiterP :: Parser ()
delimiterP = many' oneSpaceP *> string delimiter *> tillEndOfLine

upToDelimiterP :: Parser a -> Parser [a]
upToDelimiterP p = ([] <$ delimiterP) <|> ((:) <$> p <*> upToDelimiterP p)

oneSpaceP :: Parser Char
oneSpaceP = char ' '

anyStringP :: Parser Text
anyStringP = takeWhile1 (not . isSpace)

parseLine :: Parser Text
parseLine = many' oneSpaceP *> anyStringP <* tillEndOfLine

tillEndOfLine :: Parser ()
tillEndOfLine = () <$ many' oneSpaceP <* endOfLine

manyTillEndOfLine1 :: Parser ()
manyTillEndOfLine1 = () <$ many1' tillEndOfLine
