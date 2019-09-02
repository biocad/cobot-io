module Bio.FASTA.Writer
  ( fastaToText
  ) where

import           Bio.FASTA.Type     (Fasta, FastaItem(..))
import           Bio.Sequence       (BareSequence, sequ)
import           Control.Lens       ((^.))
import           Data.Text          (Text, pack, append)
import           Data.Vector        (Vector, toList)
import Data.List.Split (chunksOf)
import           Prelude     hiding (drop)

fastaToText :: Fasta Char -> Text
fastaToText f = mconcat $ map writeItem f

writeItem :: FastaItem Char -> Text
writeItem (FastaItem name s) = ">" <> name <> "\n" <> seq2Text s

seq2Text :: BareSequence Char -> Text
seq2Text s = pack $ vector2Text $ s ^. Bio.Sequence.sequ

vector2Text :: Vector Char -> String
vector2Text v = concatMap (++ "\n") $ chunksOf 80 $ toList v
