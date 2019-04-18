module Bio.ABI.Extract 
  ( extract
  , extract'
  ) where

import           Bio.Sequence               (Sequence, Weighted (..))
import           Data.Array                 (listArray)
import           Data.ByteString            as BS (ByteString)
import           Data.ByteString.Lazy       as BSL (ByteString, fromStrict)
import           Data.ByteString.Lazy.Char8 as BSL8 (unpack)
import           Data.Char                  (ord)
import           Data.List                  (elem, find)
import           Data.Maybe                 (maybe)
import           Data.Text                  (Text)
import           Hyrax.Abif                 (Abif (..), Directory (..))
import           Hyrax.Abif.Read            (getAbif)

-- | Extracts data from 'Data.ByteString.Lazy.ByteString', it should be ABI file.
--
extract :: BSL.ByteString -> Either Text (Sequence (Weighted Char))
extract bs = do
    abif      <- getAbif bs
    sequence' <- extractSequence abif
    quality'  <- extractQuality  abif
    let list   = zipWith Weighted sequence' quality' :: [Weighted Char]
    pure . listArray (0, length list - 1) $ list

-- | Extracts data from 'Data.ByteString.ByteString', it should be ABI file.
--
extract' :: BS.ByteString -> Either Text (Sequence (Weighted Char))
extract' = extract . BSL.fromStrict

-------------------------------------------------------------------------------
-- INTERNAL
-------------------------------------------------------------------------------

extractQuality :: Abif -> Either Text [Double]
extractQuality abif = map (fromIntegral . ord) <$> findDataByDirectory "PCON" abif

extractSequence :: Abif -> Either Text String
extractSequence abif = findDataByDirectory "PBAS" abif >>= checkACGT

checkACGT :: String -> Either Text String
checkACGT str | all validChar str = Right str
              | otherwise         = Left "Bio.ABI.Extract: could not parse sequence"
  where
    validChar :: Char -> Bool
    validChar ch = ch `elem` ['A', 'C', 'G', 'T']

findDataByDirectory :: Text -> Abif -> Either Text String
findDataByDirectory dirName abif =
    let directoryM = find (\Directory{..} -> dTagName == dirName) . aDirs $ abif
    in maybe (Left errorMsg) (Right . getData) directoryM
  where
    errorMsg :: Text
    errorMsg = "Bio.ABI.Extract: could not find directory " <> dirName

    getData :: Directory -> String
    getData = BSL8.unpack . dData
