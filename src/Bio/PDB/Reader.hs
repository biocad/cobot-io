module Bio.PDB.Reader
  ( fromTextPDB
  , fromFilePDB
  , PDBWarnings(..)
  ) where

import           Bio.PDB.Parser         (pdbP)
import           Bio.PDB.Type           (PDB (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.List              as L (findIndices, length)
import           Data.Maybe             (catMaybes)
import           Data.Text              as T (Text, length, lines, pack,
                                              replicate, take, unlines)
import qualified Data.Text.IO           as TIO (readFile)


type LineNumber = Int

data PDBWarnings = LineTooLong LineNumber
                 | LineTooShort LineNumber
  deriving (Show, Eq)

standardizeText :: Text -> ([PDBWarnings], Text)
standardizeText text = (textWarnings, T.unlines standardizedLines)
  where
    textLines = T.lines text
    desiredLength = 80  -- cause it is max length in standart pdb file

    warnings'n'text = map standardizeLine $ zip [0..] textLines
    textWarnings = catMaybes (fst <$> warnings'n'text)
    standardizedLines = snd <$> warnings'n'text

    standardizeLine :: (Int, Text) -> (Maybe PDBWarnings, Text)
    standardizeLine (lineNumber,line) | lineLength < desiredLength = (Just (LineTooShort lineNumber), line <> T.replicate spacesCount " ")
                                      | lineLength > desiredLength = (Just (LineTooLong lineNumber), T.take desiredLength line)
                                      | otherwise = (Nothing, line)
      where
        lineLength = T.length line
        spacesCount = desiredLength - lineLength


isMdlLine :: Text -> Bool
isMdlLine line = elem (T.take 6 line) modelStrings || elem (T.take 5 line) modelStrings
  where
    modelStrings = ["MODEL ", "ENDMDL", "ATOM ", "TER   ", "HETATM", "ANISOU", "CONECT"]

checkRow :: [Int] -> Bool
checkRow [] = True
checkRow row@(x : xs) = last row - x + 1 == L.length row

checkMdlLines :: ([PDBWarnings], Text) -> Bool
checkMdlLines warnings'n'text = checkRow mdlLineNumbers
  where
    mdlLineNumbers = findIndices isMdlLine $ T.lines (snd warnings'n'text)

preprocess :: Text -> Either Text ([PDBWarnings], Text)
preprocess text = do
  let standardizedText = standardizeText text
  if checkMdlLines standardizedText
  then Right standardizedText
  else Left "There are trash strings between model strings"


fromFilePDB :: MonadIO m => FilePath -> m (Either Text ([PDBWarnings], PDB))
fromFilePDB = liftIO . fmap fromTextPDB . TIO.readFile

fromTextPDB :: Text -> Either Text ([PDBWarnings], PDB)
fromTextPDB text = do
  (warnings, preprocessedText) <- preprocess text
  pdb <- first pack $ parseOnly pdbP preprocessedText

  pure (warnings, pdb)
