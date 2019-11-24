module Bio.FileFormat.FASTA where

import           Control.Lens         (makeLenses, (^.))
import           Control.Monad.Except
import qualified Data.Map             as M
import           Data.Text            (Text)

import           Bio.IR.ISequence
import           Bio.IR.Meta

data Fasta = Fasta { _fname :: Text
                   , _fsequ :: Text
                   }
  deriving (Show, Eq)

makeLenses ''Fasta

instance ISequenceLike Fasta where
    fromISequence                = const $ throwError MissingMeta
    toISequence (Fasta _ s)      = pure $ mkISequence s
    fromISequenceMeta (Meta m d) = case M.lookup MetaName m of
                                     Just (T name) -> pure $ Fasta name (d ^. sequ)
                                     _             -> throwError $ WrongMeta MetaName
    toISequenceMeta (Fasta n s)  = pure $ mkMeta [MetaName =: n] (mkISequence s)
