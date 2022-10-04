{-# LANGUAGE CPP #-}

module Bio.FASTA
  ( module T
  , WritableFastaToken (..)
  , fromFile
  , toFile
  , fastaP
  , fastaLine
  , modificationP
  , Parser
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.IO           (readFile, writeFile)
import System.FilePath        (takeBaseName)
import Text.Megaparsec        (errorBundlePretty, parse)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (..))
import Prelude            hiding (fail, readFile, writeFile)
#else
import Prelude hiding (readFile, writeFile)
#endif

import Bio.FASTA.Parser
import Bio.FASTA.Type   as T
import Bio.FASTA.Writer (WritableFastaToken (..), fastaToText)

-- | Reads 'FastaSequence' from given file.
--
fromFile :: (MonadFail m, MonadIO m) => FilePath -> m (Fasta Char)
fromFile f = liftIO (readFile f) >>= either (fail . errorBundlePretty) pure . parse fastaP (takeBaseName f)

-- | Writes 'FastaSequence' to file.
--
toFile :: MonadIO m => Fasta Char -> FilePath -> m ()
toFile s f = liftIO $ writeFile f $ fastaToText s
