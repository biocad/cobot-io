{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bio.PDB.Writer
  ( pdbToFile
  , pdbToText
  ) where


import           Bio.PDB.Type           (Atom (..), Model, PDB (..))
import           Control.Monad          (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T (cons, init, intercalate, last,
                                              length, pack, replicate,
                                              singleton)
import qualified Data.Text.IO           as TIO (writeFile)
import qualified Data.Vector            as V (fromList, length, toList, zip)
import           Text.Printf            (printf)

pdbToFile :: MonadIO m => FilePath -> PDB -> m ()
pdbToFile path = liftIO . TIO.writeFile path . pdbToText

pdbToText :: PDB -> Text
pdbToText PDB{..} = (<> newLine <> end)
                  $ T.intercalate newLine . V.toList . fmap (modelToText separateModels)
                  $ V.zip models $ V.fromList [1 ..]
  where
    separateModels = V.length models > 1

    end :: Text
    end = "END   "

modelToText :: Bool -> (Model, Int) -> Text
modelToText separateModels (pdbModel, modelInd) = modelPrefix <> atomsT <> modelSuffix
  where
    atomsT = T.intercalate newLine . V.toList . fmap atomToText . join $ pdbModel

    modelPrefix | separateModels = mdl <> spaceText 4 <> prependToS 4 modelInd <> newLine
                | otherwise      = ""

    modelSuffix | separateModels = endmdl <> newLine
                | otherwise      = ""

    mdl :: Text
    mdl = "MODEL "

    endmdl :: Text
    endmdl = "ENDMDL "

-- TODO: chainToText
chainToText :: Int -> Vector Atom -> Text
chainToText startInd atoms = undefined

atomToText :: Atom -> Text
atomToText Atom{..} = res
  where
    recordName | isHetatm atomResName = hetatm
               | otherwise            = atm

    serial     = prependToS 5 atomSerial
    name       = (\t -> if T.last t == space then T.cons space $ T.init t else t) $ appendTo 4 atomName
    altLoc     = T.singleton atomAltLoc
    resName    = prependTo 3 atomResName
    chainID    = T.singleton atomChainID
    resSeq     = prependToS 4 atomResSeq
    iCode      = T.singleton atomICode
    x          = prependTo 8 $ printFloat 3 atomX
    y          = prependTo 8 $ printFloat 3 atomY
    z          = prependTo 8 $ printFloat 3 atomZ
    occupancy  = prependTo 6 $ printFloat 2 atomOccupancy
    tempFactor = prependTo 6 $ printFloat 2 atomTempFactor
    element    = prependTo 2 atomElement

    charge | atomCharge /= zeroCharge = prependTo 2 atomCharge
           | otherwise                = spaceText 2

    res = recordName <> serial <> spaceText 1 <> name <> altLoc
                     <> resName <> spaceText 1 <> chainID <> resSeq <> iCode <> spaceText 3
                     <> x <> y <> z <> occupancy <> tempFactor <> spaceText 10
                     <> element <> charge

    atm :: Text
    atm = "ATOM  "

    hetatm :: Text
    hetatm = "HETATM"

    zeroCharge :: Text
    zeroCharge = "0"

    printFloat :: Int -> Float -> Text
    printFloat after f = T.pack $ printf "%.*f" after f

    isHetatm :: Text -> Bool
    isHetatm = (`notElem` canonicalAminoAcids)
      where
        canonicalAminoAcids = [ "ACE", "ALA", "ARG", "ASN", "ASP", "CYS", "GLU", "GLN"
                              , "GLY", "HIS", "HID", "HIE", "HIP", "ILE", "LEU", "LYS", "LYN"
                              , "MET", "NMA", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL"
                              ]

prependToS :: Show a => Int -> a -> Text
prependToS i = prependTo i . T.pack . show

prependTo :: Int -> Text -> Text
prependTo i t = spaceText (i - T.length t) <> t

appendTo :: Int -> Text -> Text
appendTo i t = t <> spaceText (i - T.length t)

newLine :: Text
newLine = "\n"

spaceText :: Int -> Text
spaceText = flip T.replicate " "

space :: Char
space = ' '
