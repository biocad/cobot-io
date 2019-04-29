module Bio.Uniprot
  ( module T
  , parseRecord
  , fetch
  ) where

import           Data.Text.Encoding     ( decodeUtf8 )
import           Data.String            ( IsString(..) )
import           Data.Attoparsec.Text   ( parseOnly )
import           Control.Monad.IO.Class ( MonadIO )
import           Network.HTTP.Simple    ( httpBS, getResponseBody )

import           Bio.Uniprot.Type       as T
import           Bio.Uniprot.Parser

-- | Fetches Uniprot record from Uniprot
fetch :: MonadIO m => String -> m Record
fetch recid = do let url = fromString $ "https://www.uniprot.org/uniprot/" <> recid <> ".txt"
                 resp <- httpBS url
                 case parseOnly parseRecord (decodeUtf8 $ getResponseBody resp) of
                   Left err -> fail err
                   Right r  -> pure r
