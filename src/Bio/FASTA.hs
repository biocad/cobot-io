module Bio.FASTA
  ( module T
  , fromFile
  , toFile
  , fastaP
  ) where

import           Bio.FASTA.Parser
import           Bio.FASTA.Type             as T
import           Bio.FASTA.Writer           (fastaToText)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Attoparsec.Text       (parseOnly)
import           Data.Text.IO               (readFile, writeFile)
import           Prelude            hiding  (writeFile, readFile)

-- | Reads 'FastaSequence' from given file.
--
fromFile :: MonadIO m => FilePath -> m (Fasta Char)
fromFile f = liftIO (readFile f) >>= either fail pure . parseOnly fastaP

-- | Writes 'FastaSequence' to file.
--
toFile :: MonadIO m => Fasta Char -> FilePath -> m ()
toFile s f = liftIO $ writeFile f $ fastaToText s
