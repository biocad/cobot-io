module Bio.MAE
  ( module T
  , fromFile
  , fromText
  , maeP
  ) where

import           Bio.MAE.Parser
import           Bio.MAE.Type           (Mae)
import qualified Bio.MAE.Type           as T
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.Text              (Text, pack)
import qualified Data.Text.IO           as TIO (readFile)

-- | Reads 'Mae' from givem file.
--
fromFile :: MonadIO m => FilePath -> m Mae
fromFile f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly maeP

-- | Reads 'Mae' from 'Text'.
--
fromText :: Text -> Either Text Mae
fromText = first pack . parseOnly maeP
