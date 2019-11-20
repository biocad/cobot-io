{-# LANGUAGE OverloadedStrings #-}

module Bio.PDB.Parser
  ( Atom(..)
  , CoordLike(..)
  , atomP
  , hetatmP
  , coordLikeP
  , manyModelsP
  , titleP
  , pdbP
  , otherFieldP
  , PdbData
  )
where


import           Bio.PDB.Type
import           Control.Applicative  (many, some, (<|>))
import           Control.DeepSeq      ()
import           Data.Array           ()
import           Data.Attoparsec.Text (Parser, anyChar, count, decimal,
                                       endOfLine, isEndOfLine, many', satisfy,
                                       skipWhile, space, string, take,
                                       takeWhile, takeWhile1, choice)
import           Data.Char            (isSpace)
import           Data.List            (groupBy)
import           Data.Map             (fromListWithKey)
import           Data.Map.Strict      (Map, unionWith, empty)
import           Text.Read            (readMaybe)
import           Data.Text            (Text, concat, empty, isPrefixOf, lines,
                                       null, pack, strip, unpack, append)
import           Data.Vector          (Vector, fromList, singleton, (++), empty)
import           GHC.Generics         ()
import Debug.Trace

manySpaces :: Parser ()
manySpaces = () <$ (many' $ satisfy isSpace)

-- textWithSpacesP :: Parser Text
-- textWithSpacesP = Data.Attoparsec.Text.takeWhile (`notElem` ['\n', '\r'])

atomP :: Parser CoordLike
atomP = let atom = Atom <$>
                    (string "ATOM  " *> (read <$> count 5 anyChar) <* space)
                    <*> Data.Attoparsec.Text.take 4
                    <*> anyChar
                    <*> Data.Attoparsec.Text.take 3 <* space
                    <*> anyChar
                    <*> (read <$> count 4 anyChar)
                    <*> anyChar <* count 3 space
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 6 anyChar)
                    <*> (read <$> count 6 anyChar) <* count 10 space
                    <*> Data.Attoparsec.Text.take 2
                    <*> Data.Attoparsec.Text.take 2 <* endOfLine
        in AtomLine <$> atom

  where
    textP = takeWhile1 $ not . isSpace

hetatmP :: Parser CoordLike
hetatmP = do
  _ <- string "HETATM"
  skipWhile $ not . isEndOfLine
  endOfLine
  return HetatomLine

terP :: Parser CoordLike
terP = do
  _ <- string "TER "
  skipWhile $ not . isEndOfLine
  endOfLine
  return TerLine

anisouP :: Parser CoordLike
anisouP = do
  _ <- string "ANISOU"
  skipWhile $ not . isEndOfLine
  endOfLine
  return AnisouLine

data CoordLike = AtomLine Atom | HetatomLine | TerLine | AnisouLine
  deriving (Show)

coordLikeP :: Parser [CoordLike]
coordLikeP = some (hetatmP <|> atomP <|> terP <|> anisouP)

chainsP :: Parser (Vector Chain)
chainsP = do
      coordLikeLines <- coordLikeP
      let atoms = map (\(AtomLine x) -> x) $ filter isAtom coordLikeLines
      let chains = fromList (map fromList $ groupByChains atoms)
      pure chains

      where
        isAtom :: CoordLike -> Bool
        isAtom line = case line of
          AtomLine _ -> True
          _          -> False

        groupByChains :: [Atom]-> [[Atom]]
        groupByChains = groupBy (\x y ->  atomChainID x == atomChainID y)

modelP :: Parser Model
modelP = do
  _ <- string "MODEL"
  skipWhile $ not . isEndOfLine
  endOfLine
  chains <- chainsP
  string "ENDMDL" >> skipWhile (not . isEndOfLine)
  endOfLine
  pure chains

manyModelsP :: Parser PdbData
manyModelsP = do
  models <- (:[]) <$> chainsP <|> some modelP
  return $ ModelData (fromList models)

titleStringP :: Parser Text
titleStringP = do
  _ <- string "TITLE "
  titlePart <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  pure titlePart

titleP :: Parser PdbData
titleP =  do
  titleText <- Data.Text.concat <$> some titleStringP
  return $ TitleData titleText

remarkStringP :: Parser PdbData
remarkStringP = do
 _ <- string "REMARK"
 _ <- space
 (remarkCode :: RemarkCode) <- (readMaybe <$> count 3 anyChar)
 _ <- anyChar
 remarkText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
 endOfLine
 pure $ RemarkData (remarkCode, remarkText)

fromRevListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromRevListWith f xs = fromListWithKey (\_ x y -> f y x) xs

skipConectP :: Parser ()
skipConectP = do
  _ <- string "CONECT"
  skipWhile (not . isEndOfLine) >> endOfLine
  return ()

isModelString :: Text -> Bool
isModelString s = isPrefixOf "MODEL" s || isPrefixOf "ENDMDL" s || isPrefixOf "ATOM" s || isPrefixOf "TER" s || isPrefixOf "HETATM" s || isPrefixOf "ANISOU" s

isOtherFieldString :: Text -> Bool
isOtherFieldString s = not (Data.Text.null s || isModelString s)

otherFieldP :: Parser PdbData
otherFieldP = do
    (fieldType :: Maybe FieldType) <- (readMaybe <$> count 6 anyChar) 
    fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
    endOfLine 
    return $ OtherFieldData (fieldType, fieldTypeText)

data PdbData =  ModelData (Vector Model) | OtherFieldData (Maybe FieldType, Text) | RemarkData (RemarkCode, Text) | TitleData Text
  deriving (Show)

pdbP :: Parser PDB
pdbP = do
  pdbData <- many (choice [titleP, remarkStringP, manyModelsP, otherFieldP]) -- порядок важен
  let models = foldr (Data.Vector.++) Data.Vector.empty (map (\(ModelData x) -> x) $ filter isModel pdbData) -- vector models
      otherFieldsList = map (\(OtherFieldData (Just x, y)) -> (x, singleton y)) $ filter isOtherField pdbData -- [(fieldtype, text)]
      title = foldr (Data.Text.append) "" (map (\(TitleData x) -> x) $ filter isTitle pdbData)
      remarks = fromRevListWith (Data.Vector.++) (map (\(RemarkData (x, y)) -> (x, singleton y)) $ filter isRemark pdbData)

      otherFieldsMap = fromRevListWith (Data.Vector.++) otherFieldsList

  return $ PDB title models remarks otherFieldsMap
  where
    isModel :: PdbData -> Bool
    isModel elem = case elem of
      ModelData _ -> True
      _       -> False
      
    isOtherField :: PdbData -> Bool
    isOtherField elem = case elem of
      OtherFieldData (Just v, _) -> True
      _   -> False
    
    isTitle :: PdbData -> Bool
    isTitle elem = case elem of
      TitleData _ -> True
      _ -> False
    
    isRemark :: PdbData -> Bool
    isRemark elem = case elem of
      RemarkData _ -> True
      _ -> False



-- fieldP :: Parser FieldType
-- fieldP = do
--     fieldStr <- count 6 anyChar
--     let field = strip fieldStr -- delete trailing spaces
--     read field


