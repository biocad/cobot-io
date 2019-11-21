module Bio.MAE.Type
  ( Mae (..)
  , Block (..)
  , Table (..)
  , MaeValue (..)
  , FromMaeValue (..)
  ) where

import           Data.Map.Strict (Map)
import           Data.Maybe      (fromJust)
import           Data.Text       (Text)

data Mae = Mae { version :: Text
               , blocks  :: [Block]
               }
  deriving (Eq, Show)

data Block = Block { blockName :: Text
                   , fields    :: Map Text MaeValue
                   , tables    :: [Table]
                   }
  deriving (Eq, Show)

data Table = Table { tableName :: Text
                   , contents  :: Map Text [MaeValue]
                   }
  deriving (Eq, Show)                   

data MaeValue = IntMaeValue Int
              | RealMaeValue Float
              | StringMaeValue Text
              | BoolMaeValue Bool
              | Absent
  deriving (Eq, Show)

class FromMaeValue a where
    fromMaeValue :: MaeValue -> Maybe a

    unsafeFromMaeValue :: MaeValue -> a
    unsafeFromMaeValue = fromJust . fromMaeValue

instance FromMaeValue Int where
    fromMaeValue :: MaeValue -> Maybe Int
    fromMaeValue (IntMaeValue i) = Just i
    fromMaeValue _               = Nothing

instance FromMaeValue Float where
    fromMaeValue :: MaeValue -> Maybe Float
    fromMaeValue (RealMaeValue f) = Just f
    fromMaeValue _                = Nothing

instance FromMaeValue Bool where
    fromMaeValue :: MaeValue -> Maybe Bool
    fromMaeValue (BoolMaeValue b) = Just b
    fromMaeValue _                = Nothing

instance FromMaeValue Text where
    fromMaeValue :: MaeValue -> Maybe Text
    fromMaeValue (StringMaeValue t) = Just t
    fromMaeValue _                  = Nothing
