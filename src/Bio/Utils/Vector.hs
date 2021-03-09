module Bio.Utils.Vector 
  ( (!?!)
  ) where


import GHC.Stack             (HasCallStack)
import Data.Vector           (Vector)
import qualified Data.Vector as V ((!?),
                                   length)
import Data.Maybe            (fromMaybe)

infix 9 !?!
(!?!) :: (HasCallStack, Show a) => Vector a -> Int -> a
(!?!) v i = fromMaybe (error msg) $ v V.!? i
  where
    msg :: String
    msg = "cobot-io: index " ++ show i ++ " is out of bounds. Vector length is : " ++ show (V.length v)
