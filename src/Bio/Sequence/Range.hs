module Bio.Sequence.Range 
  ( Range (..)
  , Border (..)
  , RangeBorder (..)
  , borderType
  , borderLocation
  , location
  , lower
  , upper
  , before
  , after
  , ranges
  , range
  , checkRange
  , shiftRange
  , mapRange
  , swapRange
  , point
  , preciseSpan
  , between
  ) where

import Control.DeepSeq (NFData)
import Control.Lens    (makeLenses)
import GHC.Generics    (Generic)

data Border
  = Precise
  | Exceeded
  deriving (Eq, Show, Generic, NFData)

data RangeBorder
  = RangeBorder
      { _borderType     :: Border
      , _borderLocation :: Int
      }
  deriving (Eq, Show, Generic, NFData)

makeLenses ''RangeBorder

data Range
  = Point
      { _location :: Int
      }
  -- ^ The exact location of a single base feature
  -- Example in GB:  conf            258
  | Span
      { _lower :: RangeBorder
      , _upper :: RangeBorder
      }
  -- ^ A region consisting of a simple span of bases.
  -- The symbols `<`and `>' are used to indicate that the beginning or end of the
  -- feature is beyond the range of the presented sequence.
  -- Examples in GB: tRNA            1..87
  --                 tRNA            <1..87     
  --                 tRNA            1..>87  
  | Between
      { _before :: Int
      , _after  :: Int
      }
  -- ^ The feature is between bases.
  -- Example in GB:  misc_recomb     105^106
  | Join
      { _ranges :: [Range]
      }
  -- ^ The feature consists of the union of several ranges.
  -- Example in GB:  origin          join(1, 23..50, 77..>100)
  | Complement
      { _range :: Range
      }
  -- ^ Indicates that the range is complementary.
  -- Example in GB:  rep             complement(69..420)
  deriving (Eq, Show, Generic, NFData)

makeLenses ''Range

point :: Int -> Range
point = Point

preciseSpan :: (Int, Int) -> Range
preciseSpan (lo, hi) = Span (RangeBorder Precise lo) (RangeBorder Precise hi)

between :: (Int, Int) -> Range
between = uncurry Between

checkRange :: Int -> Range -> Bool
checkRange len (Point pos) = 0 <= pos && pos <= len
checkRange len (Span (RangeBorder _ lInd) (RangeBorder _ rInd)) = lInd < rInd && 0 <= lInd && rInd <= len
checkRange len (Between lInd rInd) = lInd < rInd && 0 <= lInd && rInd <= len
checkRange len (Join ranges') = all (checkRange len) ranges'
checkRange len (Complement range') = checkRange len range'

mapRange :: (Int -> Int) -> Range -> Range
mapRange f (Point pos) = Point (f pos)
mapRange f (Span (RangeBorder bLo lo) (RangeBorder bHi hi)) = Span (RangeBorder bLo (f lo)) (RangeBorder bHi (f hi))
mapRange f (Between lo hi) = Between (f lo) (f hi)
mapRange f (Join ranges') = Join $ fmap (mapRange f) ranges'
mapRange f (Complement range') = Complement $ mapRange f range'

shiftRange :: Int -> Range -> Range
shiftRange delta = mapRange (+ delta) 

swapRange :: Range -> Range
swapRange r@Point{}           = r
swapRange (Span brLo brHi)    = Span brHi brLo
swapRange (Between lo hi)     = Between hi lo
swapRange (Join ranges')      = Join $ fmap swapRange ranges'
swapRange (Complement range') = Complement $ swapRange range'

