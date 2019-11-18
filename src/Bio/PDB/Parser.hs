{-# LANGUAGE OverloadedStrings #-}

module Bio.PDB.Parser
  ( Atom(..)
  , CoordLike(..)
  , atomP
  , hetatmP
  , coordLikeP
  , manyModelsP
  , titleP
  , remarkP
  , pdbP
  )
where


import           Bio.PDB.Type
import           Control.Applicative  (many, some, (<|>))
import           Control.DeepSeq      ()
import           Data.Array           ()
import           Data.Attoparsec.Text (Parser, anyChar, count, decimal,
                                       endOfLine, isEndOfLine, many', satisfy,
                                       skipWhile, space, string, take,
                                       takeWhile, takeWhile1)
import           Data.Char            (isSpace)
import           Data.List            (groupBy)
import           Data.Map             (fromListWithKey)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text, concat, pack)
import           Data.Vector          (Vector, fromList, singleton, (++))
import           GHC.Generics         ()


manySpaces :: Parser ()
manySpaces = () <$ (many' $ satisfy isSpace)

-- textWithSpacesP :: Parser Text
-- textWithSpacesP = Data.Attoparsec.Text.takeWhile (`notElem` ['\n', '\r'])

atomP :: Parser CoordLike
atomP = let atom = Atom <$>
                    (string "ATOM" *> manySpaces *> decimal <* manySpaces)
                    <*> Data.Attoparsec.Text.take 4
                    <*> anyChar
                    <*> textP <* manySpaces
                    <*> anyChar <* manySpaces
                    <*> decimal
                    <*> anyChar <* count 3 space
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 8 anyChar)
                    <*> (read <$> count 6 anyChar)
                    <*> (read <$> count 6 anyChar) <* manySpaces
                    <*> textP
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

manyModelsP :: Parser [Model]
manyModelsP =  (:[]) <$> chainsP <|> many modelP

titleStringP :: Parser Text
titleStringP = do
  _ <- string "TITLE "
  titlePart <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  pure titlePart

titleP :: Parser Text
titleP = Data.Text.concat <$> some titleStringP

remarkStringP :: Parser (RemarkCode, Text)
remarkStringP = do
  _ <- string "REMARK"
  manySpaces
  remarkCode <- decimal
  _ <- space
  remarkPart <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  pure (remarkCode, remarkPart)

fromRevListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromRevListWith f xs = fromListWithKey (\_ x y -> f y x) xs

remarkP :: Parser (Map RemarkCode RemarkData)
remarkP = do
  codeRemarkList <- some remarkStringP
  pure $ fromRevListWith (Data.Vector.++) ( map (\(x, y) -> (x, singleton y)) codeRemarkList)

otherFieldsStringP :: FieldType -> Parser (FieldType, Text)
otherFieldsStringP fieldType = do
  _ <- string (pack $ show fieldType)
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

coordTransformStringP :: FieldType -> Parser (FieldType, Text)
coordTransformStringP fieldType = do
  _ <- string (pack $ init (show fieldType))
  _ <- anyChar
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

skipConectP :: Parser ()
skipConectP = do
  _ <- string "CONECT"
  skipWhile (not . isEndOfLine) >> endOfLine
  return ()

dbrefnStringP :: FieldType -> Parser (FieldType, Text)
dbrefnStringP fieldType= do
  _ <- string (pack $ show fieldType)
  _ <- space
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

hetStringP :: FieldType -> Parser (FieldType, Text)
hetStringP fieldType= do
  _ <- string (pack $ show fieldType)
  _ <- space
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

pdbP :: Parser PDB
pdbP = do
    header <- some (otherFieldsStringP HEADER)
    obslte <- many (otherFieldsStringP OBSLTE)
    title   <- Data.Text.concat <$> some titleStringP
    split <- many (otherFieldsStringP SPLIT)
    caveat  <-  many (otherFieldsStringP CAVEAT)
    compnd  <-  some (otherFieldsStringP COMPND)
    source  <-  some (otherFieldsStringP SOURCE)
    keywds  <-  some (otherFieldsStringP KEYWDS)
    expdta  <-  some (otherFieldsStringP EXPDTA)
    nummdl  <-  many (otherFieldsStringP NUMMDL)
    mdltyp  <-  many (otherFieldsStringP MDLTYP)
    author  <-  some (otherFieldsStringP AUTHOR)
    revdat  <-  some (otherFieldsStringP REVDAT)
    sprsde  <-  many (otherFieldsStringP SPRSDE)
    jrnl    <-  many (otherFieldsStringP JRNL)
    remarksMap <- remarkP
    dbref   <- many (dbrefnStringP DBREF)
    dbref1  <-  many (dbrefnStringP DBREF1)
    dbref2  <-  many (dbrefnStringP DBREF2)
    seqadv  <-  many (otherFieldsStringP SEQADV)
    seqres  <-  some (otherFieldsStringP SEQRES)
    modres  <-  many (otherFieldsStringP MODRES)
    het     <- many (hetStringP HET)
    hetnam  <-  many (hetStringP HETNAM)
    hetsyn  <-  many (hetStringP HETSYN)
    formul  <-  many (otherFieldsStringP FORMUL)
    helix   <- many (otherFieldsStringP HELIX)
    sheet   <- many (otherFieldsStringP SHEET)
    ssbond <-  many (otherFieldsStringP SSBOND)
    link    <- many (otherFieldsStringP LINK)
    cispep  <-  many (otherFieldsStringP CISPEP)
    site    <- many (otherFieldsStringP SITE)
    cryst1  <-  some (coordTransformStringP CRYST1)
    origxn  <-  some (coordTransformStringP ORIGXn)
    scalen  <-  some (coordTransformStringP SCALEn)
    mtrixn  <-  many (coordTransformStringP MTRIXn)
    modelsVector <- fromList <$> manyModelsP -- fromList  исправить на many, так как  может не быть ни одной модели
    _ <- many skipConectP
    master  <- (:[]) <$> otherFieldsStringP MASTER

    let otherFieldsList = header Prelude.++ obslte Prelude.++ split Prelude.++ caveat Prelude.++ compnd
              Prelude.++ source Prelude.++ keywds Prelude.++ expdta Prelude.++ nummdl
              Prelude.++ mdltyp Prelude.++ author Prelude.++ revdat Prelude.++ sprsde
              Prelude.++ jrnl Prelude.++ dbref Prelude.++ dbref1 Prelude.++ dbref2
              Prelude.++ seqadv Prelude.++ seqres Prelude.++ modres Prelude.++ het
              Prelude.++ hetnam Prelude.++ hetsyn Prelude.++ formul Prelude.++ helix
              Prelude.++ sheet Prelude.++ ssbond Prelude.++ link Prelude.++ cispep
              Prelude.++ site Prelude.++ cryst1 Prelude.++ origxn Prelude.++ scalen
              Prelude.++ mtrixn Prelude.++ master

    let otherFieldsMap = fromRevListWith (Data.Vector.++) ( map (\(x, y) -> (x, singleton y)) otherFieldsList)
    pure (PDB title modelsVector remarksMap otherFieldsMap)






