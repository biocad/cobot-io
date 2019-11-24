module Bio.IR.ISequence where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Lens (makeLenses, view)
import Control.Monad.Except

import Bio.IR.Meta

data ISequence = ISequence { _sequ    :: Text
                           , _marking :: [(Range, Label)]
                           , _weights :: Vector Int
                           }
  deriving (Show, Eq)

makeLenses ''ISequence

mkISequence :: Text -> ISequence
mkISequence s = ISequence s [] V.empty

class ISequenceLike a where
    fromISequence :: MonadError MetaError m => ISequence -> m a
    fromISequence = fromISequenceMeta . pure

    toISequence :: MonadError MetaError m => a -> m ISequence
    toISequence = fmap (view mdata) . toISequenceMeta

    fromISequenceMeta :: MonadError MetaError m => Meta ISequence -> m a
    fromISequenceMeta = fromISequence . view mdata

    toISequenceMeta :: MonadError MetaError m => a -> m (Meta ISequence)
    toISequenceMeta = fmap pure . toISequence

