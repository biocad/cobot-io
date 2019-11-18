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
import           Control.DeepSeq      (NFData (..))
import           Data.Array           (Array, array, elems, listArray)
import           Data.Attoparsec.Text (Parser, anyChar, count, decimal,
                                       endOfLine, isEndOfLine, many', rational,
                                       satisfy, skipSpace, skipWhile, space,
                                       string, take, takeTill, takeWhile,
                                       takeWhile1)
import           Data.Char            (isSpace)
import           Data.List            (groupBy)
import           Data.Map             (fromAscListWith, fromListWith,
                                       fromListWithKey)
import           Data.Map.Strict      (Map, union)
import           Data.Text            (Text, append, concat, pack, unpack)
import           Data.Vector          (Vector, fromList, singleton, (++))
import           GHC.Generics         (Generic)
import           Debug.Trace
import Data.Typeable



manySpaces :: Parser ()
manySpaces = () <$ (many' $ satisfy isSpace)

textWithSpacesP :: Parser Text
textWithSpacesP = Data.Attoparsec.Text.takeWhile (`notElem` ['\n', '\r'])


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
        string "HETATM"
        skipWhile $ not . isEndOfLine
        endOfLine
        return HetatomLine

terP :: Parser CoordLike
terP = do
     string "TER "
     skipWhile $ not . isEndOfLine
     endOfLine
     return TerLine

data CoordLike = AtomLine Atom | HetatomLine | TerLine
  deriving (Show)

coordLikeP :: Parser [CoordLike]
coordLikeP = some (hetatmP <|> atomP <|> terP)

chainsP :: Parser (Vector Chain)
chainsP = do
      lines <- coordLikeP
      let atoms = map (\(AtomLine x) -> x) $ filter isAtom lines
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
    string "MODEL"
    skipWhile $ not . isEndOfLine
    endOfLine
    chains <- chainsP
    string "ENDMDL" >> skipWhile (not . isEndOfLine) >> endOfLine
    pure chains

manyModelsP :: Parser [Model]
manyModelsP =  (:[]) <$> chainsP <|> many modelP

titleStringP :: Parser Text
titleStringP = do
  string "TITLE "
  titlePart <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  pure titlePart

titleP :: Parser Text
titleP = Data.Text.concat <$> some titleStringP

remarkStringP :: Parser (RemarkCode, Text)
remarkStringP = do
  string "REMARK"
  manySpaces
  remarkCode <- decimal
  space
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
  string (pack $ show fieldType)
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

coordTransformStringP :: FieldType -> Parser (FieldType, Text)
coordTransformStringP fieldType = do
  string (pack $ init (show fieldType))
  anyChar
  fieldTypeText <- Data.Attoparsec.Text.takeWhile $ not . isEndOfLine
  endOfLine
  return (fieldType, fieldTypeText)

skipConectP :: Parser ()
skipConectP = do 
  string "CONECT" 
  skipWhile (not . isEndOfLine) >> endOfLine
  return ()

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
    dbref   <- many (otherFieldsStringP DBREF)
    dbref1  <-  many (otherFieldsStringP DBREF1)
    dbref2  <-  many (otherFieldsStringP DBREF2)
    seqadv  <-  many (otherFieldsStringP SEQADV)
    seqres  <-  some (otherFieldsStringP SEQRES)
    modres  <-  many (otherFieldsStringP MODRES)
    het     <- many (otherFieldsStringP HET)
    hetnam  <-  many (otherFieldsStringP HETNAM)
    hetsyn  <-  many (otherFieldsStringP HETSYN)
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
    many skipConectP
    master  <- (:[]) <$> otherFieldsStringP MASTER

    traceShowM $ header
    let otherFieldsList = header Prelude.++ obslte Prelude.++ caveat Prelude.++ compnd
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
  





