module Bio.Utils.Map 
  ( (!?!)
  ) where


import GHC.Stack       (HasCallStack)
import Data.Map.Strict (Map, 
                        (!?))
import Data.Maybe      (fromMaybe)

infix 9 !?!
(!?!) :: (HasCallStack, Ord k, Show k, Show a) => Map k a -> k -> a
(!?!) m k = fromMaybe (error $ "cobot-io: No key " ++ show k ++ " in Map: " ++ show m) $ m !? k
