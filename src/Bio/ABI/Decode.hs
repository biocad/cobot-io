{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.ABI.Decode
  ( decodeRawSequence
  , decodeRawSequence'
  ) where

import           Control.Applicative        (many)
import           Data.Bifunctor             (bimap)
import           Data.Binary.Get            (getInt16be, runGetOrFail)
import           Data.ByteString            as BS (ByteString)
import           Data.ByteString.Lazy       as BSL (ByteString, fromStrict)
import           Data.ByteString.Lazy.Char8 as BSL8 (unpack)
import           Data.Char                  (ord)
import           Data.Functor               ((<&>))
import           Data.Int                   (Int16)
import           Data.List                  (find)
import           Data.Text                  (Text, pack)
import           Data.Vector                (Vector, fromList)
import           Hyrax.Abif                 (Abif (..), Directory (..))
import           Hyrax.Abif.Read            (getAbif)

import           Bio.Sequence               (SequenceDecodable (..),
                                             weightedSequence)
import           Bio.Sequence.Basecalled    (BasecalledSequence,
                                             BasecalledSequenceWithRawData (..))

-- | Decode ABIF file with additional raw data that may be required for later processing.
decodeRawSequence :: BSL.ByteString -> Either Text BasecalledSequenceWithRawData
decodeRawSequence bs = do
    abif <- getAbif bs
    sequence' <- extractSequence abif
    quality' <- extractQuality abif
    bsSequence <- weightedSequence sequence' quality'

    bsRawG <- findDataByDirectory "DATA" 9 abif >>= decodeShortArray
    bsRawA <- findDataByDirectory "DATA" 10 abif >>= decodeShortArray
    bsRawT <- findDataByDirectory "DATA" 11 abif >>= decodeShortArray
    bsRawC <- findDataByDirectory "DATA" 12 abif >>= decodeShortArray

    bsPeakLocations <- findDataByDirectory "PLOC" 2 abif >>= decodeShortArray <&> fmap fromIntegral

    return BasecalledSequenceWithRawData{..}

-- | Same as 'decodeRawSequence', for strict @ByteString@.
decodeRawSequence' :: BS.ByteString -> Either Text BasecalledSequenceWithRawData
decodeRawSequence' = decodeRawSequence . BSL.fromStrict

-- | Discards raw data information.
instance SequenceDecodable BasecalledSequenceWithRawData BasecalledSequence where
  sequenceDecode = pure . bsSequence

-- | Converts 'Data.ByteString.Lazy.ByteString' (that should be content of ABI file)
-- into 'BasecalledSequence'.
--
instance SequenceDecodable BSL.ByteString BasecalledSequence where
    sequenceDecode :: BSL.ByteString -> Either Text BasecalledSequence
    sequenceDecode bs = do
        abif      <- getAbif bs
        sequence' <- extractSequence abif
        quality'  <- extractQuality  abif
        weightedSequence sequence' quality'

-- | Converts 'Data.ByteString.ByteString' (that should be content of ABI file)
-- into 'BasecalledSequence'.
--
instance SequenceDecodable BS.ByteString BasecalledSequence where
    sequenceDecode :: BS.ByteString -> Either Text BasecalledSequence
    sequenceDecode = sequenceDecode . BSL.fromStrict

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

-- | Extracts sequence from ABI file.
--
extractSequence :: Abif -> Either Text String
extractSequence abif = findDataByDirectory "PBAS" 1 abif <&> BSL8.unpack . dData >>= checkACGT

-- | Extracts quality from ABI file.
-- Number are encoded with letters, thus we have function @fromIntegral . ord@.
--
extractQuality :: Abif -> Either Text [Double]
extractQuality abif = map (fromIntegral . ord) . BSL8.unpack . dData <$> findDataByDirectory "PCON" 1 abif

-- | Checks that all chars are from alphabet ACGT
--
checkACGT :: String -> Either Text String
checkACGT str | all validChar str = Right str
              | otherwise         = Left "Bio.ABI.Decode: could not parse sequence"
  where
    validChar :: Char -> Bool
    validChar ch = ch `elem` ['A', 'C', 'G', 'T']

-- | Looks into ABI file and extract data by 'Directory' name.
--
findDataByDirectory
  :: Text -- ^ Directory name
  -> Int  -- ^ 1-based directory index
  -> Abif
  -> Either Text Directory
findDataByDirectory dirName dirIndex abif =
    let directoryM = find (\Directory{..} -> dTagName == dirName && dTagNum == dirIndex) . aDirs $ abif
    in maybe (Left errorMsg) Right directoryM
  where
    errorMsg :: Text
    errorMsg = "Bio.ABI.Decode: could not find directory " <> dirName <> " with index " <> pack (show dirIndex)

decodeShortArray :: Directory -> Either Text (Vector Int16)
decodeShortArray =
  bimap
    (\(_, _, msg) -> "Data.ABI.Decode: could not decode short array: " <> pack msg)
    (\(_, _, lst) -> fromList lst)
  . runGetOrFail (many getInt16be)
  . dData
