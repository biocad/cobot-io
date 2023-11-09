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
  , extendRight
  , extendLeft
  , overlap
  , rangeMargins
  ) where

import Control.DeepSeq (NFData)
import Control.Lens    (makeLenses)
import GHC.Generics    (Generic)

-- | The type of range border. A border is @Exceeded@ when its end point is beyond the
-- specified base number, otherwise it is @Precise@.
-- In GenBank, for example, @Exceeded@ borders are marked with < and >.
--
data Border
  = Precise
  | Exceeded
  deriving (Eq, Ord, Show, Generic, NFData)

-- | The end point of a range with indication whether it is @Precise@ of @Exceeded@ (see @Border@).
--
data RangeBorder
  = RangeBorder
      { _borderType     :: Border
      , _borderLocation :: Int
      }
  deriving (Eq, Ord, Show, Generic, NFData)

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
  deriving (Eq, Ord, Show, Generic, NFData)

makeLenses ''Range

point :: Int -> Range
point = Point

preciseSpan :: (Int, Int) -> Range
preciseSpan (lo, hi) = Span (RangeBorder Precise lo) (RangeBorder Precise hi)

between :: (Int, Int) -> Range
between = uncurry Between

checkRange :: Int -> Range -> Bool
checkRange len (Point pos) = 0 <= pos && pos < len
checkRange len (Span (RangeBorder _ lInd) (RangeBorder _ rInd)) = lInd < rInd && 0 <= lInd && rInd < len
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

extendRight :: Int -> Range -> Range
extendRight delta (Point a) = Span (RangeBorder Precise a) (RangeBorder Precise (a + delta))
extendRight delta (Span lo (RangeBorder r hi)) = Span lo (RangeBorder r (hi + delta))
extendRight _ b@Between{} = b
extendRight delta (Join ranges') = Join $ extendRight delta <$> ranges'
extendRight delta (Complement range') = Complement $ extendRight delta range'

extendLeft :: Int -> Range -> Range
extendLeft delta (Point a) = Span (RangeBorder Precise (a - delta)) (RangeBorder Precise a)
extendLeft delta (Span (RangeBorder r lo) hi) = Span (RangeBorder r (lo - delta)) hi
extendLeft _ b@Between{} = b
extendLeft delta (Join ranges') = Join $ extendLeft delta <$> ranges'
extendLeft delta (Complement range') = Complement $ extendLeft delta range'

overlap :: Range -> Range -> Bool
overlap (Point a) (Point b) = a == b
overlap (Point a) (Span (RangeBorder _ lo) (RangeBorder _ hi)) = lo <= a && a <= hi
overlap (Point _) (Between _ _) = False

overlap (Span (RangeBorder _ lo1) (RangeBorder _ hi1)) (Span (RangeBorder _ lo2) (RangeBorder _ hi2)) = 
    (lo1 <= lo2 && hi1 >= lo2) ||
    (lo1 >= lo2 && lo1 <= hi2) ||
    (lo1 <= lo2 && hi1 >= hi2)
overlap (Span (RangeBorder _ lo) (RangeBorder _ hi)) (Between b1 b2) = b1 >= lo && b2 <= hi

overlap b1@Between{} b2@Between{} = b1 == b2

overlap r1 (Join ranges') = any (overlap r1) ranges'
overlap r1 (Complement range') = overlap r1 range'

overlap r1 r2 = overlap r2 r1

rangeMargins :: Range -> (Int, Int)
rangeMargins rng = 
    case rng of
      Point x -> (x, x)
      Span (RangeBorder _ lo) (RangeBorder _ hi) -> (lo, hi)
      Between lo hi -> (lo, hi)
      Join children -> let (los, his) = unzip (rangeMargins <$> children) 
                        in (minimum los, maximum his)
      Complement child -> rangeMargins child

