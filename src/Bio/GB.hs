{-# LANGUAGE CPP #-}

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
import           Data.Bifunctor         (first)
import           Data.Text              (Text, pack)
import qualified Data.Text.IO           as TIO (readFile, writeFile)
import           Text.Megaparsec        (eof, errorBundlePretty, parse)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (..))
import Prelude            hiding (fail)
#endif

-- | Reads 'GenBankSequence' from givem file.
--
fromFile :: (MonadFail m, MonadIO m) => FilePath -> m GenBankSequence
fromFile f = liftIO (TIO.readFile f) >>= either (fail . errorBundlePretty) pure . parse (genBankP <* eof) ""

-- | Writes 'GenBankSequence' to file.
--
toFile :: MonadIO m => GenBankSequence -> FilePath -> m ()
toFile s f = liftIO $ TIO.writeFile f $ genBankToText s

-- | Reads 'GenBankSequence' from 'Text'.
--
fromText :: Text -> Either Text GenBankSequence
fromText = first (pack . errorBundlePretty) . parse (genBankP <* eof) ""

-- | Writes 'GenBankSequence' to 'Text'.
--
toText :: GenBankSequence -> Text
toText = genBankToText
