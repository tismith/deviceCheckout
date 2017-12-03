{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Database.SQLite.Simple (NamedParam(..),
    queryNamed, query_, executeNamed, execute, withConnection)
import Web.Scotty (scotty, get, delete, post, json, jsonData,
    param, status, ScottyM, ActionM, finish, liftAndCatchIO, defaultHandler, rescue)
import Network.HTTP.Types (notFound404, internalServerError500, badRequest400, Status)
import GHC.Generics (Generic)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text)
import Data.Aeson (FromJSON, ToJSON)

--Local imports
import Bug (Bug(..))

--TL.Text to line up with what Scott exception handlers expect
data AppError = AppError {
    errorMessage :: TL.Text
} deriving (Show, Generic)
instance ToJSON AppError
instance FromJSON AppError

defaultDB :: String
defaultDB = "bugs.db"

main :: IO ()
main = do
    scotty 3000 $ defaultHandler (jsonError internalServerError500) >> routes defaultDB

--default error handler, return the message in json
jsonError :: Status -> TL.Text -> ActionM a
jsonError errorCode m = do
    status errorCode
    json $ AppError m
    finish

routes :: String -> ScottyM ()
routes db = do
    get "/api/bugs" $ do
        bugs <- liftAndCatchIO $ withConnection db (\conn ->
            query_ conn "SELECT * FROM bugs" :: IO [Bug])
        json bugs

    get "/api/bugs/:bug" $ do
        bug <- (param "bug" :: ActionM T.Text) `rescue` jsonError badRequest400
        r <- liftAndCatchIO $ withConnection db (\conn ->
            queryNamed conn
                "SELECT * FROM bugs WHERE jira_id = :id" [":id" := bug] :: IO [Bug])
        case r of
            (b:_) -> json b
            _ -> status notFound404

    delete "/api/bugs/:bug" $ do
        bug <- (param "bug" :: ActionM T.Text) `rescue` jsonError badRequest400
        liftAndCatchIO $ withConnection db (\conn ->
            executeNamed conn "DELETE FROM bugs WHERE jira_id = :id" [":id" := bug])

    post "/api/bugs" $ do
        request <- (jsonData :: ActionM Bug) `rescue` jsonError badRequest400
        liftAndCatchIO $ withConnection db $ \conn ->
            execute conn "INSERT INTO bugs (jira_id, url, jira_status, assignment, test_status, comments) values (?, ?, ?, ?, ?, ?)" request
        json request

