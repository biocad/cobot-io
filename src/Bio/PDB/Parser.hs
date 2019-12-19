{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.PDB.Parser
  ( pdbP )
where


import           Bio.PDB.Type         (Atom (..), Chain, FieldType, Model,
                                       PDB (..), RemarkCode)
import           Control.Applicative  (many, some, (<|>))
import           Control.DeepSeq      ()
import           Data.Attoparsec.Text (Parser, choice, count, endOfLine,
                                       isEndOfLine, satisfy, skipWhile, space,
                                       string, takeWhile, endOfInput)
import qualified Data.List            as L (groupBy)
import           Data.Map.Strict      (Map, fromListWithKey)
import           Data.Maybe           (catMaybes)
import           Data.Monoid          ((<>))
import           Data.Text            as T (Text, concat, pack, stripEnd)
import qualified Data.Vector          as V (Vector, empty, fromList, singleton)
import           GHC.Generics         ()
import           Text.Read            (readMaybe)

pdbP :: Parser PDB
pdbP = do
    pdbData <- many (choice [titleP, remarkStringP, manyModelsP, otherFieldP]) -- parser order is important
    let models = foldr (<>) V.empty $ catMaybes (getModels <$> pdbData)
        otherFieldsMap = fromRevListWith (<>) $ catMaybes (getOtherField <$> pdbData)
        title = foldr (<>) "" $ catMaybes (getTitle <$> pdbData)
        remarks = fromRevListWith (<>) $ catMaybes (getRemarks <$> pdbData)

    return $ PDB title models remarks otherFieldsMap
    where
      getModels :: PdbData -> Maybe (V.Vector Model)
      getModels item = case item of
        ModelData x -> Just x
        _           -> Nothing

      getOtherField :: PdbData -> Maybe (FieldType, V.Vector Text)
      getOtherField item = case item of
        OtherFieldData (Just x, y) -> Just (x, V.singleton y)
        _                          -> Nothing

      getTitle :: PdbData -> Maybe Text
      getTitle item = case item of
        TitleData x -> Just x
        _           -> Nothing

      getRemarks :: PdbData -> Maybe (RemarkCode, V.Vector Text)
      getRemarks item = case item of
        RemarkData (x, y) -> Just (x, V.singleton y)
        _                 -> Nothing

data PdbData = ModelData (V.Vector Model)
             | OtherFieldData (Maybe FieldType, Text)
             | RemarkData (RemarkCode, Text)
             | TitleData Text
  deriving (Show)

notEndLineChar :: Parser Char
notEndLineChar = satisfy $ not . isEndOfLine

takeText :: Parser Text
takeText = Data.Attoparsec.Text.takeWhile $ not . isEndOfLine

atomP :: Parser CoordLike
atomP = let atom = Atom <$>
                    (string "ATOM " *>                                       -- (1 -  6) # we increased atomSerial field for one symbol
                    (read <$> count 6 notEndLineChar) <* space)              -- (7 - 11)  atomSerial
                    <*> (T.pack <$> count 4 notEndLineChar)                  -- (13 - 16) atomName
                    <*> notEndLineChar                                       -- (17)      atomAltLoc
                    <*> (T.pack <$> count 3 notEndLineChar) <* space         -- (18 - 20) atomResName
                    <*> notEndLineChar                                       -- (22)      atomChainID
                    <*> (read <$> count 4 notEndLineChar)                    -- (23 - 26) atomResSeq
                    <*> notEndLineChar <* count 3 space                      -- (27)      atomICode
                    <*> (read <$> count 8 notEndLineChar)                    -- (31 - 38) atomX
                    <*> (read <$> count 8 notEndLineChar)                    -- (39 - 46) atomY
                    <*> (read <$> count 8 notEndLineChar)                    -- (47 - 54) atomZ
                    <*> (read <$> count 6 notEndLineChar)                    -- (55 - 60) atomOccupancy
                    <*> (read <$> count 6 notEndLineChar) <* count 10 space  -- (61 - 66) atomTempFactor
                    <*> (T.pack <$> count 2 notEndLineChar)                  -- (77 - 78) atomElement
                    <*> (T.pack <$> count 2 notEndLineChar)                  -- (79 - 80) atomCharge
                    <* (endOfLine <|> endOfInput)
        in AtomLine <$> atom

coordNotAtomP :: Parser CoordLike
coordNotAtomP = do
    _ <- string "HETATM" <|> string "TER " <|> string "ANISOU" <|> string "CONECT"
    skipWhile $ not . isEndOfLine
    endOfLine
    return CoordNotAtomLine

data CoordLike = AtomLine Atom | CoordNotAtomLine
  deriving (Show)

coordLikeP :: Parser [CoordLike]
coordLikeP = some (coordNotAtomP <|> atomP)

chainsP :: Parser (V.Vector Chain)
chainsP = do
    coordLikeLines <- coordLikeP
    let atoms = catMaybes (getAtom <$> coordLikeLines)
    let chains = V.fromList (map V.fromList $ groupByChains atoms)
    pure chains
    where
      getAtom :: CoordLike -> Maybe Atom
      getAtom line = case line of
        AtomLine x -> Just x
        _          -> Nothing
      groupByChains :: [Atom]-> [[Atom]]
      groupByChains = L.groupBy (\x y ->  atomChainID x == atomChainID y)

modelP :: Parser Model
modelP = do
    _ <- string "MODEL"
    skipWhile $ not . isEndOfLine
    endOfLine
    chains <- chainsP
    string "ENDMDL" >> skipWhile (not . isEndOfLine)
    endOfLine <|> endOfInput
    pure chains

manyModelsP :: Parser PdbData
manyModelsP = do
    models <- (:[]) <$> chainsP <|> some modelP
    return $ ModelData (V.fromList models)

titleStringP :: Parser Text
titleStringP = do
    _ <- string "TITLE "
    titleText <- takeText
    endOfLine
    pure $ T.stripEnd titleText

titleP :: Parser PdbData
titleP =  do
    titleText <- T.concat <$> some titleStringP
    return $ TitleData titleText

remarkStringP :: Parser PdbData
remarkStringP = do
    _ <- string "REMARK"
    _ <- space
    (remarkCode :: RemarkCode) <- readMaybe <$> count 3 notEndLineChar
    _ <- notEndLineChar
    remarkText <- takeText
    endOfLine
    pure $ RemarkData (remarkCode, T.stripEnd remarkText)

fromRevListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromRevListWith f xs = fromListWithKey (\_ x y -> f y x) xs

otherFieldP :: Parser PdbData
otherFieldP = do
    (fieldType :: Maybe FieldType) <- readMaybe <$> count 6 notEndLineChar
    fieldTypeText <- takeText
    endOfLine <|> endOfInput
    return $ OtherFieldData (fieldType,  T.stripEnd fieldTypeText)
