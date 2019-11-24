module Bio.IR.Meta where

import           Control.Lens         (makeLenses)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

type Label = Text

data Range = Range { _start     :: Int
                   , _end       :: Int
                   , _isReverse :: Bool
                   }
  deriving (Show, Eq, Ord)

makeLenses ''Range

data Value = T Text
           | I Int
           | F Float
           | L [Value]
           | M (Map MetaKey Value)
  deriving (Show, Eq)

data MetaKey = MetaLabel Label
             | MetaName
             | MetaRange Range
  deriving (Show, Eq, Ord)

type MetaMap = Map MetaKey Value

-------------------------------------------------------
-- MetaKeyable

class MetaKeyable a where
  toMetaKey :: a -> MetaKey
  fromMetaKey :: MetaKey -> Maybe a

instance MetaKeyable Text where
  toMetaKey = MetaLabel
  fromMetaKey (MetaLabel n) = Just n
  fromMetaKey MetaName      = Just "name"
  fromMetaKey _             = Nothing

instance MetaKeyable Range where
  toMetaKey = MetaRange
  fromMetaKey (MetaRange r) = Just r
  fromMetaKey _             = Nothing

instance MetaKeyable MetaKey where
  toMetaKey = id
  fromMetaKey = pure

----------------------------------------------
-- Valueable

class Valueable a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance Valueable Text where
  toValue = T
  fromValue (T x) = Just x
  fromValue _     = Nothing

instance Valueable Int where
  toValue = I
  fromValue (I x) = Just x
  fromValue _     = Nothing

instance Valueable Float where
  toValue = F
  fromValue (F x) = Just x
  fromValue _     = Nothing

instance Valueable a => Valueable [a] where
  toValue = L . fmap toValue
  fromValue (L xs) = traverse fromValue xs
  fromValue _      = Nothing

instance Valueable a => Valueable (Map MetaKey a) where
  toValue = M . fmap toValue
  fromValue (M x) = traverse fromValue x
  fromValue _     = Nothing

instance Valueable Value where
  toValue = id
  fromValue = pure

infix 1 =:

(=:) :: (MetaKeyable k, Valueable a) => k -> a -> (MetaKey, Value)
k =: v = (toMetaKey k, toValue v)

forceJustMapKeys :: Ord a => Map (Maybe a) b -> Maybe (Map a b)
forceJustMapKeys m = case M.lookup Nothing m of
                       Just _  -> Nothing
                       Nothing -> Just (M.mapKeys fromJust m)

forceJustMapVals :: Eq b => Map a (Maybe b) -> Maybe (Map a b)
forceJustMapVals m = if Nothing `elem` m then Nothing
                                         else Just (fmap fromJust m)

--------------------
-- Meta

data Meta a = Meta { _meta  :: MetaMap
                   , _mdata :: a
                   }
  deriving (Show, Eq, Functor, Foldable)

makeLenses ''Meta

instance Applicative Meta where
  pure                    = Meta M.empty
  Meta m1 f <*> Meta m2 x = Meta (m1 <> m2) (f x)

instance Traversable Meta where
  sequenceA (Meta m d) = Meta m <$> d

mkMeta :: [(MetaKey, Value)] -> a -> Meta a
mkMeta = Meta . M.fromList

------------------------
-- MetaError
data MetaError = MissingMeta
               | WrongMeta MetaKey
  deriving (Show, Eq)

-------------------------

getMetaField :: forall a m.(Valueable a, MonadError MetaError m) => MetaKey -> MetaMap -> m a
getMetaField key m = case M.lookup key m >>= fromValue of
                       Just x  -> pure x
                       Nothing -> throwError $ WrongMeta key
