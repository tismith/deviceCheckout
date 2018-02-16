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
import qualified Data.Text.Lazy as TL (Text, unpack, length)

--Concurrency
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

--Option parsing
import Options.Applicative (Parser, execParser, strOption, long,
    short, metavar, value, showDefault, help, auto, option, info,
    (<**>), helper, fullDesc, progDesc)
import Data.Semigroup ((<>))

--Utilities
import Data.Maybe (isNothing, fromMaybe, isJust)
import Text.Read (readMaybe)

--Local imports
import Types (Device(..), DeviceUpdate(..),
    ReservationStatus(..), ApplicationOptions(..))
import Database (getDevices, updateDevice, getDevice, deleteDevice, addDevice)
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
          <> value "devices.db"
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
            (fullDesc <> progDesc "Basic device registry database webapp")
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
    get "/devices" $ do
        devices <- withDatabase getDevices
                `rescue` textError internalServerError500
        html $ deviceList devices

    post "/devices" $ do
        rawDeviceName <- (param "deviceName" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawOwner <- (param "deviceOwner" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawReservationStatus <- (param "reservationStatus" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawComments <- (param "comments" :: ActionD TL.Text)
            `rescue` textError badRequest400

        let maybeReservationStatus =
                readMaybe (TL.unpack rawReservationStatus) :: Maybe ReservationStatus
        when (isNothing maybeReservationStatus)
            $ textError badRequest400 "Invalid reservationStatus"

        when (isJust maybeReservationStatus && TL.length rawOwner == 0)
            $ textError badRequest400 "You need to supply a username"

        let realReservationStatus = fromMaybe Available maybeReservationStatus

        --Blank out the user on returning devices
        let realOwner =
                case realReservationStatus of
                    Available -> Nothing
                    _ -> Just rawOwner

        -- Blank out comments on returning devices
        let realComments =
                case realReservationStatus of
                    Available -> Nothing
                    _ -> Just rawComments

        let deviceUpdate = DeviceUpdate
                            rawDeviceName
                            realOwner
                            realReservationStatus
                            realComments
        updateReturn <- withDatabase (`updateDevice` deviceUpdate)
                `rescue` textError internalServerError500
        case updateReturn of
            Left e -> textError internalServerError500 e
            Right _ -> redirect "/devices"

    get "/api/devices" $ do
        devices <- withDatabase getDevices
        json devices

    get "/api/devices/:device" $ do
        device <- (param "device" :: ActionD TL.Text) `rescue` jsonError badRequest400
        r <- withDatabase (`getDevice` device)
        case r of
            Just b -> json b
            _ -> status notFound404

    delete "/api/devices/:device" $ do
        device <- (param "device" :: ActionD TL.Text) `rescue` jsonError badRequest400
        deleteReturn <- withDatabase (`deleteDevice` device)
        case deleteReturn of
            Left e -> textError internalServerError500 e
            Right _ -> finish

    post "/api/devices" $ do
        request <- (jsonData :: ActionD Device) `rescue` jsonError badRequest400
        addReturn <- withDatabase (`addDevice` request)
        case addReturn of
            Left e -> textError internalServerError500 e
            Right _ -> json request

    get "/" $ redirect "/devices"

    --default route, Scotty does a HTML based 404 by default
    notFound $ textError notFound404 "Not found"

