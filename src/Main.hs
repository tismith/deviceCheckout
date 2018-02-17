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

--System
import System.Exit (exitSuccess)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

--Concurrency
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
    takeMVar, putMVar, withMVar)

--Option parsing
import Options.Applicative (execParser)

--Utilities
import Data.Maybe (isNothing, fromMaybe, isJust)
import Text.Read (readMaybe)

--Local imports
import Types (Device(..), DeviceUpdate(..),
    ReservationStatus(..), ApplicationOptions(..), Application(..))
import Database (getDevices, updateDevice, getDevice, deleteDevice, addDevice)
import CmdLine (applicationOptionsParser)
import Templates

-- NB: ScottyT and hence ScottD is not a Transformer,
--     the underlying monad will only be run on a per-action basis
type ScottyD = ScottyT TL.Text (ReaderT Application IO)
type ActionD = ActionT TL.Text (ReaderT Application IO)

handler :: MVar () -> IO ()
handler mutex = putMVar mutex ()

main :: IO ()
main = do
    options <- execParser applicationOptionsParser
    let application = Application options (newMVar ())

    --start scotty in a background thread, so we can exit on a signal
    _ <- forkIO $
        scottyT (portNumber options) (passInApplication application) $ do
            defaultHandler (jsonError internalServerError500)
            routes

    --have the main thread wait on a shutdown signal, and then wait
    --for the db to be free, then exit
    shutdownMVar <- newEmptyMVar
    _ <- installHandler sigINT (Catch $ handler shutdownMVar) Nothing
    _ <- installHandler sigTERM (Catch $ handler shutdownMVar) Nothing

    --here we wait...
    _ <- takeMVar shutdownMVar
    databaseMutexMVar <- databaseMutex application
    _ <- takeMVar databaseMutexMVar
    putStrLn "Exiting..."
    exitSuccess
    where
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
    application <- lift ask
    dbMutex <- liftAndCatchIO $ databaseMutex application
    let dbPath = (databasePath . applicationOptions) application
    liftAndCatchIO $ withMVar dbMutex (\_ -> action dbPath)

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

