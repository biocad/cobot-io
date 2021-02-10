module Bio.FASTA.Writer
  ( fastaToText
  , WritableFastaToken (..)
  ) where

import Bio.FASTA.Type  (Fasta, FastaItem (..), ModItem (..), modificationToString)
import Bio.Sequence    (BareSequence, sequ)
import Control.Lens    ((^.))
import Data.List.Split (chunksOf)
import Data.Text       (Text, pack)
import Data.Vector     (Vector, toList)
import Prelude         hiding (drop)

class WritableFastaToken a where
    tokenToString :: a -> String

instance WritableFastaToken Char where
    tokenToString = pure

instance WritableFastaToken ModItem where
    tokenToString (Letter l) = [l]
    tokenToString (Mod m)    = modificationToString m

fastaToText :: WritableFastaToken a => Fasta a -> Text
fastaToText f = mconcat $ map writeItem f

writeItem :: WritableFastaToken a => FastaItem a -> Text
writeItem (FastaItem name s) = ">" <> name <> "\n" <> seq2Text s

seq2Text :: WritableFastaToken a => BareSequence a -> Text
seq2Text s = pack $ vector2Text $ s ^. Bio.Sequence.sequ

vector2Text :: WritableFastaToken a => Vector a -> String
vector2Text v = concatMap (++ "\n") $ chunksOf 80 $ concatMap tokenToString $ toList v
