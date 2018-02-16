{-# LANGUAGE OverloadedStrings #-}

--Web and scotty
import Web.Scotty.Trans (scottyT, get, delete, post, json, jsonData,
    param, status, finish, liftAndCatchIO, defaultHandler, rescue,
    notFound, text, html, redirect, ScottyT, ActionT)
import Network.HTTP.Types (notFound404, internalServerError500,
    badRequest400, Status)
import Data.Aeson (object, (.=))

--Monads
import Control.Monad (when)
import Control.Monad.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans (lift)
import qualified Data.Text.Lazy as TL (Text, unpack)

--Concurrency
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

--Option parsing
import Options.Applicative (Parser, execParser, strOption, long,
    short, metavar, value, showDefault, help, auto, option, info,
    (<**>), helper, fullDesc, progDesc)
import Data.Semigroup ((<>))

--Utilities
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

--Local imports
import Types (Bug(..), BugUpdate(..), TestStatus, ApplicationOptions(..))
import Database (getBugs, updateBug, getBug, deleteBug, addBug)
import Templates

data Application = Application {
    _appOptions :: ApplicationOptions,
    databaseHandle :: IO (MVar String)
}

applicationOptions :: Parser ApplicationOptions
applicationOptions = ApplicationOptions
    <$> strOption
        ( long "database"
          <> short 'f'
          <> metavar "DATABASE"
          <> showDefault
          <> value "bugs.db"
          <> help "SQLite DB path" )
    <*> option auto
        ( long "webserver-port"
          <> short 'p'
          <> help "Port to run the webserver"
          <> showDefault
          <> value 3000
          <> metavar "INT" )

-- NB: ScottyT and hence ScottD is not a Transformer,
--     the underlying monad will only be run on a per-action basis
type ScottyD = ScottyT TL.Text (ReaderT Application IO)
type ActionD = ActionT TL.Text (ReaderT Application IO)

main :: IO ()
main = do
    options <- execParser opts
    let application = Application options (newMVar (databasePath options))
    scottyT (portNumber options) (passInApplication application) $ do
        defaultHandler (jsonError internalServerError500)
        routes
    where
        opts = info
            (applicationOptions <**> helper)
            (fullDesc <> progDesc "Basic bug database webapp")
        passInApplication = flip runReaderT

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

withDatabase :: (String -> IO a) -> ActionD a
withDatabase action = do
    options <- lift ask
    dbPath <- liftAndCatchIO $ databaseHandle options
    liftAndCatchIO $ modifyMVar dbPath $ \s -> do
        result <- action s
        return (s, result)

routes :: ScottyD ()
routes = do
    get "/bugs" $ do
        bugs <- withDatabase getBugs
                `rescue` textError internalServerError500
        html $ bugList bugs

    post "/bugs" $ do
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
        updateReturn <- withDatabase (`updateBug` bugUpdate)
                `rescue` textError internalServerError500
        case updateReturn of
            Left e -> textError internalServerError500 e
            Right _ -> redirect "/bugs"

    get "/api/bugs" $ do
        bugs <- withDatabase getBugs
        json bugs

    get "/api/bugs/:bug" $ do
        bug <- (param "bug" :: ActionD TL.Text) `rescue` jsonError badRequest400
        r <- withDatabase (`getBug` bug)
        case r of
            Just b -> json b
            _ -> status notFound404

    delete "/api/bugs/:bug" $ do
        bug <- (param "bug" :: ActionD TL.Text) `rescue` jsonError badRequest400
        deleteReturn <- withDatabase (`deleteBug` bug)
        case deleteReturn of
            Left e -> textError internalServerError500 e
            Right _ -> finish

    post "/api/bugs" $ do
        request <- (jsonData :: ActionD Bug) `rescue` jsonError badRequest400
        addReturn <- withDatabase (`addBug` request)
        case addReturn of
            Left e -> textError internalServerError500 e
            Right _ -> json request

    get "/" $ redirect "/bugs"
    --default route, Scotty does a HTML based 404 by default
    notFound $ textError notFound404 "Not found"

