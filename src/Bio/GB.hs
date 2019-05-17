module Bio.GB
  ( module T
  , fromFile
  , genBankP
  ) where

import           Bio.GB.Parser
import           Bio.GB.Type            as T
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO (readFile)

fromFile :: MonadIO m => FilePath -> m GenBankSequence
fromFile f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly genBankP
