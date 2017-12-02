{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Database.SQLite.Simple (Connection, open, NamedParam(..),
    queryNamed, query_, executeNamed, execute)
import Web.Scotty (scotty, get, delete, post, json, jsonData,
    param, status, ScottyM, ActionM, finish, liftAndCatchIO, defaultHandler)
import Network.HTTP.Types (notFound404, status400)
import GHC.Generics (Generic)
import Data.Text as T (Text)
import Data.Text.Lazy as TL (Text)
import Data.Aeson (FromJSON, ToJSON)

--Local imports
import Bug (Bug(..))

--TL.Text to line up with what Scott exception handlers expect
data AppError = AppError {
    errorMessage :: TL.Text
} deriving (Show, Generic)
instance ToJSON AppError
instance FromJSON AppError

main :: IO ()
main = do
    conn <- open "bugs.db"
    scotty 3000 $ (defaultHandler handleError >> routes conn)

--default error handler, return the message in json
--defaults to error 400, can individually override a statement's
--handler by using rescue from Web.Scotty
handleError :: TL.Text -> ActionM a
handleError m = do
    status status400
    json $ AppError m
    finish

routes :: Connection -> ScottyM ()
routes conn = do
    get "/api/bugs" $ do
        let q = query_ conn "SELECT * FROM bugs" :: IO [Bug]
        bugs <- liftAndCatchIO q
        json bugs

    get "/api/bugs/:bug" $ do
        bug <- param "bug" :: ActionM T.Text
        let q = queryNamed conn "SELECT * FROM bugs WHERE jira_id = :id" [":id" := bug] :: IO [Bug]
        r <- liftAndCatchIO q
        case r of
            (b:_) -> json b
            _ -> status notFound404

    delete "/api/bugs/:bug" $ do
        bug <- param "bug" :: ActionM T.Text
        let q = executeNamed conn "DELETE FROM bugs WHERE jira_id = :id" [":id" := bug]
        liftAndCatchIO q

    post "/api/bugs" $ do
        request <- jsonData :: ActionM Bug
        let q = execute conn "INSERT INTO bugs (jira_id, url, jira_status, assignment, test_status, comments) values (?, ?, ?, ?, ?, ?)" request
        liftAndCatchIO q
        json request

