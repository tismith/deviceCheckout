{-# LANGUAGE DeriveGeneric #-}
module Types (
    Bug(..),
    BugUpdate(..),
    JiraStatus,
    TestStatus,
    ApplicationOptions(..)
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

newtype ApplicationOptions = ApplicationOptions {
    databasePath :: String
} deriving (Show)

data JiraStatus = Open | Resolved | Testing | Active
    deriving (Show, Generic, Enum, Bounded, Eq, Read)
instance ToJSON JiraStatus
instance FromJSON JiraStatus
instance ToField JiraStatus where
    toField = SQLText . TL.toStrict. TL.pack . show
instance FromField JiraStatus where
    fromField (Field (SQLText "Open") _) = Ok Open
    fromField (Field (SQLText "open") _) = Ok Open
    fromField (Field (SQLText "Resolved") _) = Ok Resolved
    fromField (Field (SQLText "resolved") _) = Ok Resolved
    fromField (Field (SQLText "Testing") _) = Ok Testing
    fromField (Field (SQLText "testing") _) = Ok Testing
    fromField (Field (SQLText "Active") _) = Ok Active
    fromField (Field (SQLText "active") _) = Ok Active
    fromField f = returnError ConversionFailed f "Invalid value for jiraStatus"

data TestStatus = Pass | Fail | Ignore
    deriving (Show, Generic, Enum, Bounded, Eq, Read)
instance ToJSON TestStatus
instance FromJSON TestStatus
instance ToField TestStatus where
    toField = SQLText . TL.toStrict. TL.pack . show
instance FromField TestStatus where
    fromField (Field (SQLText "Pass") _) = Ok Pass
    fromField (Field (SQLText "pass") _) = Ok Pass
    fromField (Field (SQLText "Fail") _) = Ok Fail
    fromField (Field (SQLText "fail") _) = Ok Fail
    fromField (Field (SQLText "Ignore") _) = Ok Ignore
    fromField (Field (SQLText "ignore") _) = Ok Ignore
    fromField f = returnError ConversionFailed f "Invalid value for testStatus"

data Bug = Bug {
    jiraId :: TL.Text,
    url :: Maybe TL.Text,
    jiraStatus :: Maybe JiraStatus,
    assignment :: Maybe TL.Text,
    testStatus :: Maybe TestStatus,
    comments :: Maybe TL.Text
} deriving (Show, Generic)
instance ToJSON Bug
instance FromJSON Bug
instance FromRow Bug where
    fromRow = Bug <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Bug where
    toRow (Bug ji u js a t c) = toRow (ji, u, js, a, t, c)

data BugUpdate = BugUpdate {
    newJiraId :: TL.Text,
    newAssignment :: Maybe TL.Text,
    newTestStatus :: Maybe TestStatus,
    newComments :: Maybe TL.Text
} deriving (Show, Generic)
