{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.ABI.Decode () where

import           Bio.ABI.Type               (ABIProcessed)
import           Bio.Sequence               (SequenceDecodable (..),
                                             weightedSequence)
import           Data.ByteString            as BS (ByteString)
import           Data.ByteString.Lazy       as BSL (ByteString, fromStrict)
import           Data.ByteString.Lazy.Char8 as BSL8 (unpack)
import           Data.Char                  (ord)
import           Data.List                  (elem, find)
import           Data.Maybe                 (maybe)
import           Data.Text                  (Text)
import           Hyrax.Abif                 (Abif (..), Directory (..))
import           Hyrax.Abif.Read            (getAbif)

-- | Converts 'Data.ByteString.Lazy.ByteString' (that should be content of ABI file)
-- into 'ABIProcessed'.
--
instance SequenceDecodable BSL.ByteString ABIProcessed where
    sequenceDecode :: BSL.ByteString -> Either Text ABIProcessed
    sequenceDecode bs = do
        abif      <- getAbif bs
        sequence' <- extractSequence abif
        quality'  <- extractQuality  abif
        weightedSequence sequence' quality'

-- | Converts 'Data.ByteString.ByteString' (that should be content of ABI file)
-- into 'ABIProcessed'.
--
instance SequenceDecodable BS.ByteString ABIProcessed where
    sequenceDecode :: BS.ByteString -> Either Text ABIProcessed
    sequenceDecode = sequenceDecode . BSL.fromStrict

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

-- | Extracts sequence from ABI file.
--
extractSequence :: Abif -> Either Text String
extractSequence abif = findDataByDirectory "PBAS" abif >>= checkACGT

-- | Extracts quality from ABI file.
-- Number are encoded with letters, thus we have function @fromIntegral . ord@.
--
extractQuality :: Abif -> Either Text [Double]
extractQuality abif = map (fromIntegral . ord) <$> findDataByDirectory "PCON" abif

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
findDataByDirectory :: Text -> Abif -> Either Text String
findDataByDirectory dirName abif =
    let directoryM = find (\Directory{..} -> dTagName == dirName) . aDirs $ abif
    in maybe (Left errorMsg) (Right . getData) directoryM
  where
    errorMsg :: Text
    errorMsg = "Bio.ABI.Decode: could not find directory " <> dirName

    getData :: Directory -> String
    getData = BSL8.unpack . dData
