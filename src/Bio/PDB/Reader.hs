module Bio.PDB.Reader where

import           Bio.PDB.Parser         (pdbP)
import           Bio.PDB.Type           (PDB (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.List              as L (findIndices, length)
import           Data.Text              as T (Text, append, length, lines, pack,
                                              replicate, take)
import qualified Data.Text.IO           as TIO (readFile)


type LineNumber = Int

data PDBWarnings = LineTooLong LineNumber | LineTooShort LineNumber
 deriving (Show, Eq)

standardizeLines :: Int -> [Text] -> ([PDBWarnings], Text)
standardizeLines _ [] = ([], "")
standardizeLines lineNumber (line:text) = (warnings ++ textWarnings, changedLine `T.append` "\n" `T.append` textChangedLines)
   where
       desiredLength = 80  -- cause it is max length in standart pdb file
       lineLength = T.length line
       spacesCount = desiredLength - lineLength
       (warnings, changedLine) = changeLine line
       (textWarnings, textChangedLines) = standardizeLines (lineNumber + 1) text

       changeLine :: Text -> ([PDBWarnings], Text)
       changeLine str | lineLength < desiredLength = ([LineTooShort lineNumber], str <> T.replicate spacesCount " ")
                      | lineLength > desiredLength = ([LineTooLong lineNumber], T.take desiredLength str)
                      | otherwise = ([], str)

extractText :: Either (Text, PDBWarnings) Text -> Text
extractText (Right text)     = text
extractText (Left (text, _)) = text

isMdlLine :: Text -> Bool
isMdlLine line = elem (T.take 6 line) modelStrings || elem (T.take 5 line) modelStrings
        where
            modelStrings = ["MODEL ", "ENDMDL", "ATOM ", "TER   ", "HETATM", "ANISOU", "CONECT"]

checkRow :: [Int] -> Bool
checkRow [] = True
checkRow xs = last xs - head xs + 1 == L.length xs

checkMdlLines :: ([PDBWarnings], Text) -> Bool
checkMdlLines warnings'n'text = checkRow mdlLineNumbers
    where
        mdlLineNumbers = findIndices isMdlLine $ T.lines (snd warnings'n'text)

preprocess :: [Text] -> Either Text ([PDBWarnings], Text)
preprocess textLines = do
    let standardizedLines = standardizeLines 1 textLines
    if checkMdlLines standardizedLines
    then Right standardizedLines
    else Left "There are trash strings between model strings"

fromFilePDB :: MonadIO m => FilePath -> m (Either Text ([PDBWarnings], Either Text PDB))
fromFilePDB f = do
        content <- liftIO (TIO.readFile f)
        let preprocessed = preprocess $ T.lines content
        pure $ fmap (first pack . parseOnly pdbP) <$> preprocessed

fromTextPDB :: Text -> Either Text ([PDBWarnings], Either Text PDB)
fromTextPDB text = fmap (first pack . parseOnly pdbP) <$> preprocessed
  where
    preprocessed = preprocess $ T.lines text
