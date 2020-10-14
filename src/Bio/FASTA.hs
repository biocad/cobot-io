{-# LANGUAGE CPP #-}

module Bio.FASTA
  ( module T
  , fromFile
  , toFile
  , fastaP
  , fastaPGeneric
  ) where

import           Bio.FASTA.Parser
import           Bio.FASTA.Type         as T
import           Bio.FASTA.Writer       (fastaToText)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text   (parseOnly)
import           Data.Text.IO           (readFile, writeFile)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail     (MonadFail (..))
import           Prelude                hiding (fail, readFile, writeFile)
#else
import           Prelude                hiding (readFile, writeFile)
#endif

-- | Reads 'FastaSequence' from given file.
--
fromFile :: (MonadFail m, MonadIO m) => FilePath -> m (Fasta Char)
fromFile f = liftIO (readFile f) >>= either fail pure . parseOnly fastaP

-- | Writes 'FastaSequence' to file.
--
toFile :: MonadIO m => Fasta Char -> FilePath -> m ()
toFile s f = liftIO $ writeFile f $ fastaToText s
