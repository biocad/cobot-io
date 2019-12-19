module Bio.PDB.Reader where

import           Bio.PDB.Parser         (pdbP)
import           Bio.PDB.Type           (PDB (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.Either            (lefts)
import           Data.List              (findIndices)
import           Data.List              as L (length)
import           Data.Text              as T (Text, intercalate, length, lines,
                                              pack, replicate, take)
import qualified Data.Text.IO           as TIO (readFile)


type LineNumber = Int

data PDBWarnings = LineTooLong LineNumber | LineTooShort LineNumber
 deriving (Show, Eq)

standardizeLines :: Int -> [Text] -> [Either (Text, PDBWarnings) Text]
standardizeLines _ [] = []
standardizeLines lineNumber (line:text) = [changeLine line] ++ standardizeLines (lineNumber + 1) text
   where
       desiredLength = 80  -- cause it is max length in standart pdb file
       lineLength = T.length line
       spacesCount = desiredLength - lineLength

       changeLine :: Text -> Either (Text, PDBWarnings) Text
       changeLine str | lineLength < desiredLength = Left (str <> T.replicate spacesCount " ", LineTooShort lineNumber)
                      | lineLength > desiredLength = Left (T.take desiredLength str, LineTooLong lineNumber)
                      | otherwise = Right str

composeText :: [Either (Text, PDBWarnings) Text] -> ([PDBWarnings], Text)
composeText lines'n'warnigs = (warnings, cleanedText)
        where
            warnings = snd <$> lefts lines'n'warnigs
            cleanedText = T.intercalate "\n" $ map extractText lines'n'warnigs

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

checkMdlLines :: [Either (Text, PDBWarnings) Text] -> Bool
checkMdlLines text'n'warnings = checkRow mdlLineNumbers
    where
        mdlLineNumbers = findIndices isMdlLine $ map extractText text'n'warnings

preprocess :: [Text] -> Either Text ([PDBWarnings], Text)
preprocess textLines = do
    let standardizedLines = standardizeLines 1 textLines
    if checkMdlLines standardizedLines
    then Right $ composeText standardizedLines
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
