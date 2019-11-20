module Bio.MAE.Type
  ( Mae (..)
  , Block (..)
  , Table (..)
  , Value (..)
  , fromValue
  , toValue
  ) where

import           Data.Map.Strict (Map)
import           Data.Maybe      (fromJust)
import           Data.Text       (Text)

data Mae = Mae { version :: Text
               , blocks  :: [Block]
               }
  deriving (Eq, Show)

data Block = Block { blockName :: Text
                   , fields    :: Map Text Value
                   , tables    :: [Table]
                   }
  deriving (Eq, Show)

data Table = Table { tableName :: Text
                   , contents  :: Map Text [Value]
                   }
  deriving (Eq, Show)                   

data Value = IntValue Int
           | RealValue Float
           | StringValue Text
  deriving (Eq, Show)

class IsValue a where
    toValue :: a -> Value

    fromValue :: Value -> Maybe a

    unsafeFromValue :: Value -> a
    unsafeFromValue = fromJust . fromValue

instance IsValue Int where
    toValue = IntValue

    fromValue :: Value -> Maybe Int
    fromValue (IntValue i) = Just i
    fromValue _            = Nothing

instance IsValue Float where
    toValue = RealValue

    fromValue :: Value -> Maybe Float
    fromValue (RealValue f) = Just f
    fromValue _             = Nothing

instance IsValue Text where
    toValue = StringValue

    fromValue :: Value -> Maybe Text
    fromValue (StringValue t) = Just t
    fromValue _               = Nothing
