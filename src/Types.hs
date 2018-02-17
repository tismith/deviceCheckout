{-# LANGUAGE DeriveGeneric #-}
module Types (
    Device(..),
    DeviceUpdate(..),
    ReservationStatus(..),
    ApplicationOptions(..),
    Application(..)
) where

import Database.SQLite.Simple(SQLData(..), ResultError(..))
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Database.SQLite.Simple.FromField (FromField(..), returnError)
import Database.SQLite.Simple.ToRow (ToRow(..), toRow)
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import Data.Text.Lazy as TL (Text, pack, toStrict)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.MVar (MVar)

data ApplicationOptions = ApplicationOptions {
    databasePath :: String,
    portNumber :: Int
} deriving (Show)

data Application = Application {
    applicationOptions ::ApplicationOptions,
    databaseMutex :: IO (MVar ())
}

data ReservationStatus = Available | Reserved
    deriving (Show, Generic, Enum, Bounded, Eq, Read)
instance ToJSON ReservationStatus
instance FromJSON ReservationStatus
instance ToField ReservationStatus where
    toField = SQLText . TL.toStrict. TL.pack . show
instance FromField ReservationStatus where
    fromField (Field (SQLText "available") _) = Ok Available
    fromField (Field (SQLText "Available") _) = Ok Available
    fromField (Field (SQLText "Reserved") _) = Ok Reserved
    fromField (Field (SQLText "reserved") _) = Ok Reserved
    --Make empty strings map to available
    fromField (Field (SQLText "") _) = Ok Available
    fromField f = returnError ConversionFailed f "Invalid value for reservationStatus"

data Device = Device {
    deviceName :: TL.Text,
    deviceUrl :: Maybe TL.Text,
    deviceOwner :: Maybe TL.Text,
    comments :: Maybe TL.Text,
    reservationStatus :: Maybe ReservationStatus
} deriving (Show, Generic)
instance ToJSON Device
instance FromJSON Device
instance FromRow Device where
    fromRow = Device <$> field <*> field <*> field <*> field <*> field
instance ToRow Device where
    toRow (Device a b c d e) = toRow (a, b, c, d, e)

data DeviceUpdate = DeviceUpdate {
    newDeviceName :: TL.Text,
    newOwner :: Maybe TL.Text,
    newReservationStatus :: ReservationStatus,
    newComments :: Maybe TL.Text
} deriving (Show, Generic)
