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
handleError :: (TL.Text -> ActionD ()) -> Status -> TL.Text -> ActionD a
handleError formatter errorCode msg = do
    status errorCode
    formatter msg
    finish

jsonError :: Status -> TL.Text -> ActionD a
jsonError = handleError (\m -> json $ object [ "errorMessage" .= m ])

textError :: Status -> TL.Text -> ActionD a
textError = handleError text

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
        updateReturn <- liftAndCatchIO (updateBug db bugUpdate)
                `rescue` textError internalServerError500
        case updateReturn of
            Left e -> textError internalServerError500 e
            Right _ -> redirect "/bugs"

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
        deleteReturn <- liftAndCatchIO $ deleteBug db bug
        case deleteReturn of
            (Left e) -> textError internalServerError500 e
            (Right _) -> finish

    post "/api/bugs" $ do
        db <- getDatabasePath
        request <- (jsonData :: ActionD Bug) `rescue` jsonError badRequest400
        addReturn <- liftAndCatchIO $ addBug db request
        case addReturn of
            (Left e) -> textError internalServerError500 e
            (Right _) -> json request

    --default route, Scotty does a HTML based 404 by default
    notFound $ textError notFound404 "Not found"

