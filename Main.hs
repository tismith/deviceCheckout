{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty (scotty, get, json, param)
import GHC.Generics
import Data.Text as T
import Data.Aeson (FromJSON, ToJSON)

data JiraStatus = Open | Resolved | Testing | Active deriving (Show, Generic)
instance ToJSON JiraStatus
instance FromJSON JiraStatus

data TestStatus = Pass | Fail deriving (Show, Generic)
instance ToJSON TestStatus
instance FromJSON TestStatus

data Bug = Bug {
    identity :: T.Text,
    url :: T.Text,
    jiraStatus :: JiraStatus,
    assignment :: Maybe T.Text,
    testStatus :: Maybe TestStatus,
    comments :: Maybe T.Text
} deriving (Show, Generic)
instance ToJSON Bug
instance FromJSON Bug

bugIM42 :: Bug
bugIM42 = Bug {
    identity = "IM-42",
    url = "https://cvs.opengear.com:8081/browe/IM-42",
    jiraStatus = Resolved,
    assignment = Nothing,
    testStatus = Nothing,
    comments = Nothing
}

bugIM1571 :: Bug
bugIM1571 = Bug {
    identity = "IM-1571",
    url = "https://cvs.opengear.com:8081/browe/IM-1571",
    jiraStatus = Resolved,
    assignment = Just "davidb",
    testStatus = Just Pass,
    comments = Just "all good"
}

bugs :: [Bug]
bugs = [ bugIM42, bugIM1571 ]

findBug :: [Bug] -> T.Text -> Maybe Bug
findBug (b:bs) bug = if (identity b == bug) then Just b else findBug bs bug
findBug [] _ = Nothing

main :: IO ()
main = scotty 3000 $ do
  get "/bugs" $ do
    json $ bugs

  get "/bugs/:bug" $ do
    bug <- param "bug"
    json $ findBug bugs bug
