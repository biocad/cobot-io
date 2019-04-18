{-# LANGUAGE TemplateHaskell #-}

module Bio.Sequence
  ( Sequence
  , Weighted (..)
  , object
  , weight
  ) where

import           Control.DeepSeq (NFData (..))
import           Control.Lens    (makeLenses)
import           Data.Array      (Array)
import           GHC.Generics    (Generic)

-- | Type alias for sequences
--
type Sequence a = Array Int a

-- | Represents something with weight (or quality); inspired by ABI and FASTQ format
--
data Weighted a
   = Weighted { _object :: a
              , _weight :: Double
              }
  deriving (Eq, Show, Generic, Functor, Foldable, NFData)

makeLenses ''Weighted
