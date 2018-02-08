{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty.Trans (scottyT, get, delete, post, json, jsonData,
    param, status, finish, liftAndCatchIO, defaultHandler, rescue,
    notFound, text, html, redirect, ScottyT, ActionT)
import Network.HTTP.Types (notFound404, internalServerError500,
    badRequest400, Status)
import qualified Data.Text.Lazy as TL (Text, unpack)
import Data.Aeson (object, (.=))
import Control.Monad (when)
import Control.Monad.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

--Local imports
import Types (Bug(..), BugUpdate(..), TestStatus, ApplicationOptions(..))
import Database (getBugs, updateBug, getBug, deleteBug, addBug)
import Templates

defaultOptions :: ApplicationOptions
defaultOptions = ApplicationOptions "bugs.db"

-- NB: ScottyT and hence ScottD is not a Transformer,
--     the underlying monad will only be run on a per-action basis
type ScottyD = ScottyT TL.Text (ReaderT ApplicationOptions IO)
type ActionD = ActionT TL.Text (ReaderT ApplicationOptions IO)

main :: IO ()
main = scottyT 3000 passInOptions $ do
        defaultHandler (jsonError internalServerError500)
        routes
        where
            passInOptions r = runReaderT r defaultOptions


--default error handler, return the message in json
jsonError :: Status -> TL.Text -> ActionD a
jsonError errorCode m = do
    status errorCode
    json $ object [ "errorMessage" .= m ]
    finish

textError :: Status -> TL.Text -> ActionD a
textError errorCode m = do
    status errorCode
    text m
    finish

getDatabasePath :: ActionD String
getDatabasePath = databasePath <$> lift ask

routes :: ScottyD ()
routes = do
    get "/bugs" $ do
        db <- getDatabasePath
        bugs <- liftAndCatchIO (getBugs db)
                `rescue` textError internalServerError500
        html $ bugList bugs

    post "/bugs" $ do
        db <- getDatabasePath
        rawJiraId <- (param "jiraId" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawAssignment <- (param "assignment" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawTestStatus <- (param "testStatus" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawComments <- (param "comments" :: ActionD TL.Text)
            `rescue` textError badRequest400

        let maybeTestStatus = if rawTestStatus == "-"
                then Nothing
                else (readMaybe (TL.unpack rawTestStatus) :: Maybe TestStatus)
        when (rawTestStatus /= "-" && isNothing maybeTestStatus)
            $ textError badRequest400 "Invalid testStatus"

        let bugUpdate = BugUpdate
                            rawJiraId
                            (Just rawAssignment)
                            maybeTestStatus
                            (Just rawComments)
        numRowsChanged <- liftAndCatchIO (updateBug db bugUpdate)
                `rescue` textError internalServerError500

        when (numRowsChanged /= 1)
            $ textError internalServerError500 "Database error"
        redirect "/bugs"

    get "/api/bugs" $ do
        db <- getDatabasePath
        bugs <- liftAndCatchIO $ getBugs db
        json bugs

    get "/api/bugs/:bug" $ do
        db <- getDatabasePath
        bug <- (param "bug" :: ActionD TL.Text) `rescue` jsonError badRequest400
        r <- liftAndCatchIO $ getBug db bug
        case r of
            Just b -> json b
            _ -> status notFound404

    delete "/api/bugs/:bug" $ do
        db <- getDatabasePath
        bug <- (param "bug" :: ActionD TL.Text) `rescue` jsonError badRequest400
        numRowsChanged <- liftAndCatchIO $ deleteBug db bug
        when (numRowsChanged /= 1) (jsonError notFound404 "Failed to delete")
        finish

    post "/api/bugs" $ do
        db <- getDatabasePath
        request <- (jsonData :: ActionD Bug) `rescue` jsonError badRequest400
        numRowsChanged <- liftAndCatchIO $ addBug db request
        when (numRowsChanged /= 1) $ jsonError internalServerError500 "Database error"
        json request

    --default route, Scotty does a HTML based 404 by default
    notFound $ textError notFound404 "Not found"

