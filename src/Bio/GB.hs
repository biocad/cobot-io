module Bio.GB
  ( module T
  , fromFile
  , toFile
  , fromText
  , toText
  , genBankP
  ) where

import           Bio.GB.Parser
import           Bio.GB.Type            as T
import           Bio.GB.Writer          (genBankToText)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Bifunctor         (first)
import           Data.Text              (Text)
import           Data.Text              (pack)
import qualified Data.Text.IO           as TIO (readFile, writeFile)

-- | Reads 'GenBankSequence' from givem file.
--
fromFile :: MonadIO m => FilePath -> m GenBankSequence
fromFile f = liftIO (TIO.readFile f) >>= either fail pure . parseOnly genBankP

-- | Writes 'GenBankSequence' to file.
--
toFile :: MonadIO m => GenBankSequence -> FilePath -> m ()
toFile s f = liftIO $ TIO.writeFile f $ genBankToText s

-- | Reads 'GenBankSequence' from 'Text'.
--
fromText :: Text -> Either Text GenBankSequence
fromText = first pack . parseOnly genBankP

-- | Writes 'GenBankSequence' to 'Text'.
--
toText :: GenBankSequence -> Text
toText = genBankToText
