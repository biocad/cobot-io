{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Bio.Uniprot.Parser where

import           Prelude              hiding (null)
import qualified Prelude              as P (concat, init, last, null, tail)

#if MIN_VERSION_base(4, 18, 0)
import           Control.Applicative  ((<|>))
#else
import           Control.Applicative  (liftA2, (<|>))
#endif

import           Bio.Uniprot.Type
import           Control.Monad        (unless)
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Char            (isSpace)
import           Data.Functor         (($>))
import           Data.Text            (Text, append, isPrefixOf, null, pack,
                                       splitOn, unpack)

-- | Describes possible name type of DE section.
data NameType = RecName | AltName | SubName | Flags | None
  deriving (Show)

-- | Parses ID line of UniProt-KB text file.
parseID :: Parser ID
parseID = do
    string "ID   "
    entryName <- pack <$> many1 (satisfy $ inClass "A-Z0-9_")
    many1 space
    status <- (string "Reviewed" $> Reviewed) <|>
              (string "Unreviewed" $> Unreviewed)
    char ';'
    many1 space
    seqLength <- decimal
    space >> string "AA."
    pure ID{..}

-- | Parses AC lines of UniProt-KB text file.
parseAC :: Parser AC
parseAC = do
    parseStartAC
    initAC <- P.concat <$> many' (parseOneAC <* endOfLine <* parseStartAC)
    lastAC <- parseOneAC
    let accessionNumbers = initAC ++ lastAC
    pure AC{..}
  where
    parseStartAC :: Parser ()
    parseStartAC = string "AC" >> count 3 space >> pure ()

    parseOneAC :: Parser [Text]
    parseOneAC = many1 $ do
        res <- pack <$> many1 (satisfy $ inClass "A-Z0-9_")
        char ';'
        option ' ' (satisfy isHorizontalSpace)
        pure res

-- | Parses 3 DT lines of UniProt-KB text file.
parseDT :: Parser DT
parseDT = do
    (dbIntegrationDate, dbName) <- parseOneDT "integrated into UniProtKB/" <* endOfLine
    (seqVersionDate, seqVersion) <- second (read . unpack) <$> parseOneDT "sequence version " <* endOfLine
    (entryVersionDate, entryVersion) <- second (read . unpack) <$> parseOneDT "entry version "
    pure DT{..}
  where
    parseOneDT :: Text -> Parser (Text, Text)
    parseOneDT txt = do
        string "DT   "
        day <- pack <$> many1 (satisfy $ inClass "A-Z0-9-")
        char ','
        many1 space
        string txt
        x <- pack <$> many1 (satisfy $ inClass "A-Za-z0-9_-")
        char '.'
        pure (day, x)

-- | Parses DE lines of UniProt-KB text file.
parseDE :: Parser DE
parseDE = do
    recName  <- optional $ parseNameDE 0 RecName
    altNames <- many' (endOfLine *> parseAltDE 0)
    subNames <- many' (endOfLine *> parseNameDE 0 SubName)
    includes <- many' (endOfLine *> parseInternal "Includes")
    contains <- many' (endOfLine *> parseInternal "Contains")
    flags    <- option [] (endOfLine *> parseFlagsDE)
    pure DE{..}
  where
    -- Parses name section like RecName, AltName or SubName.
    parseNameDE :: Int -> NameType -> Parser Name
    parseNameDE indent nameType = do
        fullName <- parseDELine indent nameType "Full"
        shortName <- many' $ endOfLine *> parseDELine indent None "Short"
        ecNumber <- many' $ endOfLine *> parseDELine indent None "EC"
        pure Name{..}

    -- Parses flag line of DE section
    parseFlagsDE :: Parser [Flag]
    parseFlagsDE = fmap (read . unpack) .
                       ("; " `splitOn`) <$> parseDELine 0 Flags ""

    -- Parses AltName lines of DE section
    parseAltDE :: Int -> Parser AltName
    parseAltDE indent =
      (Simple <$> parseNameDE indent AltName) <|>
      (Allergen <$> parseDELine indent AltName "Allergen") <|>
      (Biotech <$> parseDELine indent AltName "Biotech") <|>
      (CDAntigen <$> parseDELine indent AltName "CD_antigen") <|>
      (INN <$> parseDELine indent AltName "INN")

    -- Parses any DE line
    parseDELine :: Int -> NameType -> Text -> Parser Text
    parseDELine indent nameType tpe = do
        string "DE   "
        count indent (char ' ')
        case nameType of
          None -> string "         "
          a    -> string $ append (pack $ show a) ": "
        unless (null tpe) $ do
            string tpe
            string "="
            pure ()
        result <- pack . P.init <$> many1 (satisfy (not . isEndOfLine))
        pure . head $ " {ECO" `splitOn` result

    -- Parses internal DE entities
    parseInternal :: Text -> Parser DE
    parseInternal name = do
        string "DE   " >> string name >> char ':'
        endOfLine
        recName  <- optional $ parseNameDE 2 RecName
        altNames <- many' (endOfLine *> parseAltDE 2)
        pure $ DE recName altNames [] [] [] []

-- | Parses DE lines of UniProt-KB text file.
parseGN :: Parser [GN]
parseGN = do
    string "GN   "
    geneName <- optional $ parseDefItem "Name"
    optional $ parseBreak "GN"
    synonyms <- option [] $ parseGNList "Synonyms"
    optional $ parseBreak "GN"
    orderedLocusNames <- option [] $ parseGNList "OrderedLocusNames"
    optional $ parseBreak "GN"
    orfNames <- option [] $ parseGNList "ORFNames"
    let gn = GN{..}
    optional $ parseBreak "GN"
    rest <- option [] $ string "and" *> endOfLine *> parseGN
    pure $ gn:rest
  where
    -- Parses any list item of GN line (like `Synonyms` or `ORFNames`)
    parseGNList :: Text -> Parser [Text]
    parseGNList name = splitOn ", " <$> parseDefItem name

-- | Parses OS lines for one record of UniProt-KB text file.
parseOS :: Parser OS
parseOS = OS . pack . P.init <$> (string "OS   " >> parseMultiLineComment "OS" 3)

-- | Parser OG line of UniProt-KB text file.
parseOG :: Parser OG
parseOG = (parseOGNonPlasmid <* many' (char ' ' >> parseEvidence) <* char '.') <|>
          (Plasmid <$> parseOGPlasmid)
  where
    parseOGNonPlasmid :: Parser OG
    parseOGNonPlasmid = string "OG   " *>
      ((string "Hydrogenosome" $> Hydrogenosome) <|>
       (string "Mitochondrion" $> Mitochondrion) <|>
       (string "Nucleomorph" $> Nucleomorph) <|>
       (string "Plastid; Apicoplast" $> Plastid PlastidApicoplast) <|>
       (string "Plastid; Chloroplast" $> Plastid PlastidChloroplast) <|>
       (string "Plastid; Organellar chromatophore" $> Plastid PlastidOrganellarChromatophore) <|>
       (string "Plastid; Cyanelle" $> Plastid PlastidCyanelle) <|>
       (string "Plastid; Non-photosynthetic plastid" $> Plastid PlastidNonPhotosynthetic) <|>
       (string "Plastid" $> Plastid PlastidSimple))

    parseOGPlasmid :: Parser [Text]
    parseOGPlasmid = do
        string "OG   "
        name <- parseAnyPlasmid
        let separator = char ',' >> optional " and"
        rest <- many' $ separator *> char ' ' *> parseAnyPlasmid
        optional separator
        rest2 <- P.concat <$> many' (endOfLine *> parseOGPlasmid)
        pure $ name : rest ++ rest2

    parseAnyPlasmid :: Parser Text
    parseAnyPlasmid = parseOnePlasmid <|>
                      (("Plasmid" <* optional (char ' ' >> parseEvidence)
                                  <* optional (char '.')) $> "") -- ABSAA_ALCSP hack

    parseOnePlasmid :: Parser Text
    parseOnePlasmid = do
        string "Plasmid "
        pack <$> parsePlasmidName

    parsePlasmidName :: Parser String
    parsePlasmidName = do
        let p = many1 (satisfy $ liftA2 (&&) (notInClass ",{") (not . isEndOfLine))
        part <- p
        nextChar <- peekChar
        plasmid <- case nextChar of
          Just '{' -> parseEvidence >> optional (char '.') $> P.init part
          _        -> pure part
        pure $ if P.last plasmid == '.' then P.init plasmid else plasmid

    countElem :: Eq a => [a] -> a -> Int
    countElem []     _             = 0
    countElem (x:xs) y | x == y    = 1 + countElem xs y
                       | otherwise = countElem xs y

-- | Parser OC line of UniProt-KB text file.
parseOC :: Parser OC
parseOC = OC <$> parseNodes "OC" ';' '.'

-- | Parses OX lines of UniProt-KB text file.
parseOX :: Parser OX
parseOX = do
    string "OX   "
    databaseQualifier <- pack <$> many1 (notChar '=')
    char '='
    taxonomicCode <- pack <$> many1 (notChar ';')
    char ';'
    pure OX{..}

-- | Parses OH line of UniProt-KB text file.
parseOH :: Parser OH
parseOH = do
    string "OH   NCBI_TaxID="
    taxId <- pack <$> many1 (notChar ';')
    char ';'
    hostName' <- many' (satisfy $ not . isEndOfLine)
    let hostName = pack $ if P.null hostName'
                            then ""
                            else P.tail . P.init $ hostName'
    pure OH{..}

-- | Parses RN, RP, RC, RX, RG, RA, RT and RL lines of UniProt-KB text file.
parseRef :: Parser Reference
parseRef = do
    rn <- parseRN
    endOfLine
    rp <- parseRP
    endOfLine
    rc <- option [] (parseRCX STRAIN "RC" <* endOfLine)
    rx <- option [] (parseRCX MEDLINE "RX" <* endOfLine)
    rg <- option [] (many' $ parseRG <* endOfLine)
    ra <- option [] (parseNodes "RA" ',' ';' <* endOfLine)
    rt <- optional  (parseRT <* endOfLine)
    rl <- parseRL
    pure Reference{..}
  where
    parseRN :: Parser Int
    parseRN = do
        number <- (string "RN   [" *> decimal) <* char ']'
        -- Despite the specification, edivence may be presented here
        _ <- many' (char ' ' *> parseEvidence)
        pure number

    parseRP :: Parser Text
    parseRP = do
        string "RP   "
        pack . P.init <$> parseMultiLineComment "RP" 3

    parseRCX :: (Enum a, Show a) => a -> Text -> Parser [(a, Text)]
    parseRCX start name = do
       string name >> string "   "
       (:) <$> parseTokPair start
           <*> many' (parseBreak name *> parseTokPair start)
     where
       parseTokPair :: (Enum a, Show a) => a -> Parser (a, Text)
       parseTokPair x = foldl1 (<|>) $
                          (\x -> (x,) <$> parseDefItem (pack . show $ x)) <$> [x..]

    parseRG :: Parser Text
    parseRG = pack <$> (string "RG   " *> many1 (satisfy $ not . isEndOfLine))

    parseRT :: Parser Text
    parseRT = do
        string "RT   \""
        let p = many1 $ satisfy $ liftA2 (&&) (not . isEndOfLine) (notInClass "\"")
        referenceTitle <- (:) <$> p <*> many' (endOfLine *> string "RT  " *> p)
        string "\";"
        pure $ pack . hyphenConcat $ referenceTitle

    parseRL :: Parser Text
    parseRL = do
        string "RL   "
        pack . P.init <$> parseMultiLineComment "RL" 3

-- | Parses CC lines of UniProt-KB text file.
parseCC :: Parser CC
parseCC = do
    string "CC   -!- "
    topic <- pack <$> many1 (notChar ':')
    char ':'
    (char ' ' $> ()) <|> (endOfLine >> string "CC" >> count 7 space $> ())
    comment <- head . (" {ECO" `splitOn`) . pack <$> parseMultiLineComment "CC" 7
    pure CC{..}

-- | UniProt-KB copyright comment
copyrightCC :: Text
copyrightCC = "CC   -----------------------------------------------------------------------\nCC   Copyrighted by the UniProt Consortium, see https://www.uniprot.org/terms\nCC   Distributed under the Creative Commons Attribution (CC BY 4.0) License\nCC   -----------------------------------------------------------------------"


-- | Parses DR lines of UniProt-KB text file.
parseDR :: Parser DR
parseDR = do
    string "DR   "
    resourceAbbr <- parseToken
    char ' '
    resourceId <- parseToken
    optionalInfo <- many1 (char ' ' *> parseToken)
    pure DR{..}
  where
    parseToken :: Parser Text
    parseToken = pack <$> parseTokenStr

    parseTokenStr :: Parser String
    parseTokenStr = do
        part <- many1 (satisfy $ liftA2 (&&) (/=';') (not . isEndOfLine))
        nextChar <- peekChar
        case nextChar of
          Nothing  -> pure . P.init $ part
          Just ';' -> do
              char ';'
              nextChar <- peekChar
              case nextChar of
                Nothing -> fail "You cannot be here"
                Just c  | isSpace c -> pure part
                Just c  -> (part <>) . (';':) <$> parseTokenStr
          Just c  -> pure . P.init $ part

-- | Parses PE line of UniProt-KB text file.
parsePE :: Parser PE
parsePE = (string "PE   1: Evidence at protein level;" $> EvidenceAtProteinLevel) <|>
          (string "PE   2: Evidence at transcript level;" $> EvidenceAtTranscriptLevel) <|>
          (string "PE   3: Inferred from homology;" $> InferredFromHomology) <|>
          (string "PE   4: Predicted;" $> Predicted) <|>
          (string "PE   5: Uncertain;" $> Uncertain)

-- | Parses KW lines of UniProt-KB text file.
parseKW :: Parser KW
parseKW = KW <$> parseNodes "KW" ';' '.'

-- | Parses FT lines of UniProt-KB text file. One FT section is parsed.
parseFT :: Parser FT
parseFT = do
    string "FT   "
    keyName <- pack <$> many1 (satisfy $ inClass "A-Z_")
    many1 space
    fromEP <- parseFTEndpoint
    many1 space
    toEP <- parseFTEndpoint
    description <- filter (not . ("{ECO" `isPrefixOf`)) . splitByMagic <$>
                     ((many' (char ' ') *> parseMultiLineComment "FT" 32) <|>
                      (hyphenConcat <$> parseMultiLine "FT" 32))
    pure FT{..}
  where
    -- Parse FT endpoint
    parseFTEndpoint :: Parser Endpoint
    parseFTEndpoint = (UncertainEP <$> (char '?' *> decimal)) <|>
                      (NTerminalEP <$> (char '<' *> decimal)) <|>
                      (CTerminalEP <$> (char '>' *> decimal)) <|>
                      (ExactEP     <$> decimal) <|>
                      (char '?' $> UnknownEP)

    -- Split string to tokens by periods outside brackets.
    splitByMagic :: String -> [Text]
    splitByMagic txt = pack <$> splitStr 0 [] txt
      where
        splitStr :: Int -> String -> String -> [String]
        splitStr _ _   []           = []
        splitStr 0 acc ['.']        = [reverse acc]
        splitStr 0 acc ('.':' ':xs) = reverse acc : splitStr 0 [] xs
        splitStr 0 acc ('.':xs)     = reverse acc : splitStr 0 [] xs
        splitStr n acc ('(':xs)     = splitStr (n+1) ('(':acc) xs
        splitStr n acc (')':xs)     = splitStr (n-1) (')':acc) xs
        splitStr n acc (x:xs)       = splitStr n (x:acc) xs

-- | Parses SQ lines of UniProt-KB text file.
parseSQ :: Parser SQ
parseSQ = do
    string "SQ   SEQUENCE"
    many1 space
    len <- decimal
    space >> string "AA;"
    many1 space
    molWeight <- decimal
    space >> string "MW;"
    many1 space
    crc64 <- pack <$> many1 (satisfy $ inClass "A-F0-9")
    space >> string "CRC64;"
    endOfLine
    sequ <- pack . P.concat <$>
            many1 (skipSpace *> many1 (satisfy $ inClass "A-Z"))
    pure SQ{..}

-- | Parses end of one UniProt record.
parseEnd :: Parser ()
parseEnd = string "//" >> pure ()

-- | Parses whole UniProt-KB record.
parseRecord :: Parser Record
parseRecord = Record <$>           (parseID  <* endOfLine)
                     <*>           (parseAC  <* endOfLine)
                     <*>           (parseDT  <* endOfLine)
                     <*>           (parseDE  <* endOfLine)
                     <*> option [] (parseGN  <* endOfLine)
                     <*>           (parseOS  <* endOfLine)
                     <*> many'     (parseOG  <* endOfLine)
                     <*>           (parseOC  <* endOfLine)
                     <*>           (parseOX  <* endOfLine)
                     <*> many'     (parseOH  <* endOfLine)
                     <*> many'     (parseRef <* endOfLine)
                     <*> many'     (parseCC  <* endOfLine) <* option "" (string copyrightCC <* endOfLine)
                     <*> many'     (parseDR  <* endOfLine)
                     <*>           (parsePE  <* endOfLine)
                     <*> optional  (parseKW  <* endOfLine)
                     <*> many'     (parseFT  <* endOfLine)
                     <*>           (parseSQ  <* endOfLine)
                     <*            parseEnd

parseEvidence :: Parser Text
parseEvidence = (\x y z -> x <> y <> z) <$>
                  string "{" <*> (pack <$> many1 (notChar '}')) <*> string "}"

-- = Helper parsers

-- | Transforms any parser to a parser of maybe value.
--
-- >>> parseOnly (optional digit) "1"
-- Right (Just 1)
--
-- >>> parseOnly (optional digit) ""
-- Right Nothing
optional :: Parser a -> Parser (Maybe a)
optional par = option Nothing (Just <$> par)

-- | Parses lines, that contain nodes splitted by `del` and ended by `end`.
parseNodes :: Text          -- ^Start 2-letter mark.
           -> Char          -- ^Delimeter char, that splits the nodes.
           -> Char          -- ^Terminal char, that ends the node list.
           -> Parser [Text]
parseNodes start del end = do
    string start >> count 3 (char ' ')
    parseNodesNoStart
  where
    parseNodesNoStart :: Parser [Text]
    parseNodesNoStart = do
        part <- parseNode
        c <- char del <|> char end
        if c == del
          then do (char ' ' $> ()) <|> (endOfLine >> string start >> count 3 (char ' ') $> ())
                  (part :) <$> parseNodesNoStart
          else do nextChar <- peekChar
                  case nextChar of
                    Nothing                -> pure [part]
                    Just c | isEndOfLine c -> pure [part]
                    Just c                 -> do (x:xs) <- parseNodesNoStart
                                                 pure (part <> x : xs)

    parseNode :: Parser Text
    parseNode = pack <$> many1 (satisfy $ liftA2 (&&) (notInClass [del,end]) (not . isEndOfLine))

-- | Parses line till the end.
parseTillEnd :: Parser String
parseTillEnd = many1 $ satisfy (not . isEndOfLine)

-- | Parses multiline comment as one string.
parseMultiLineComment :: Text -> Int -> Parser String
parseMultiLineComment start skip = hyphenConcat <$>
                                     ((:) <$> parseTillEnd
                                          <*> parseMultiLine start skip)

-- | Parses multiline comment from new line.
parseMultiLine :: Text -> Int -> Parser [String]
parseMultiLine start skip = many' $ do
    endOfLine
    string start
    count (skip - 1) (char ' ') -- leave one space to separate words
    parseTillEnd

-- | Parses line break for multiline section.
parseBreak :: Text -> Parser ()
parseBreak txt = ((endOfLine >> string txt >> string "   ") <|> string " ") $> ()

-- | Parses one item like "Something=Something else;"
parseDefItem :: Text -> Parser Text
parseDefItem name = do
    string name >> char '='
    head . (" {" `splitOn`) . pack <$> parseTillChar ';'

-- | Parses line till specific char (e.g. semicolon or dot) before space/endOfLine/endOfInput.
parseTillChar :: Char -> Parser String
parseTillChar c = do
    part <- many1 $ satisfy $ liftA2 (&&) (/=c) (not . isEndOfLine)
    nextChar <- peekChar
    case nextChar of
      Nothing                -> fail "You cannot be here!"
      Just d | d == c        -> do
          char c
          nextChar <- peekChar
          case nextChar of
            Nothing -> pure part
            Just d  | isSpace d -> pure part
            Just d  -> (part <>) . (d:) <$> parseTillChar c
      Just d | isEndOfLine d -> do
          endOfLine
          count 2 anyChar
          count 2 (char ' ')
          (part <>) <$> parseTillChar c
      Just _                 -> fail "You cannot be here!"

-- | Delete needless space after hyphen on concat.
hyphenConcat :: [String] -> String
hyphenConcat []       = []
hyphenConcat [x]      = x
hyphenConcat (x:y:ys) = x ++ hyphenConcat (sy:ys)
  where
    sy :: String
    sy | last x == '-'                  = tail y
       | isAA (last x) && isAA (y !! 1) = tail y
       | otherwise                      = y

    isAA :: Char -> Bool
    isAA = inClass "A-Z"
