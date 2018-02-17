{-# LANGUAGE OverloadedStrings #-}
module Routes(routes) where

--Web and scotty
import Web.Scotty.Trans (get, delete, post, json, jsonData,
    param, status, finish, liftAndCatchIO, defaultHandler, rescue,
    notFound, text, html, redirect)
import Network.HTTP.Types (notFound404, internalServerError500,
    badRequest400, Status)
import Data.Aeson (object, (.=))

--Monads
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import qualified Data.Text.Lazy as TL (Text, unpack, length)

--Concurrency
import Control.Concurrent.MVar (withMVar)

--Utilities
import Data.Maybe (isNothing, fromMaybe, isJust)
import Text.Read (readMaybe)

--Local imports
import Types (Device(..), DeviceUpdate(..), DeviceEdit(..),
    ReservationStatus(..), ApplicationOptions(..), Application(..),
    ActionD, ScottyD)
import Database (getDevices, updateDevice, editDevice,
    getDevice, deleteDevice, addDevice)
import Templates

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
    defaultHandler (jsonError internalServerError500)
    get "/devices" $ do
        errorMessage <-
            (Just <$> param "errorMessage" :: ActionD (Maybe TL.Text))
            `rescue` const (return Nothing)
        devices <- withDatabase getDevices
                `rescue` textError internalServerError500
        html $ deviceList errorMessage devices

    get "/editDevices" $ do
        errorMessage <-
            (Just <$> param "errorMessage" :: ActionD (Maybe TL.Text))
            `rescue` const (return Nothing)
        devices <- withDatabase getDevices
                `rescue` textError internalServerError500
        html $ editDeviceList errorMessage devices

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
            $ redirect "/devices?errorMessage=You need to supply an owner"

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

    post "/editDevices" $ do
        rawDeviceName <- (param "deviceName" :: ActionD TL.Text)
            `rescue` textError badRequest400
        rawUrl <- (param "deviceUrl" :: ActionD TL.Text)
            `rescue` textError badRequest400

        when (TL.length rawDeviceName == 0)
            $ redirect "/editDevices?errorMessage=You need to supply a device name"

        let deviceUpdate = DeviceEdit
                            rawDeviceName
                            (Just rawUrl)
        updateReturn <- withDatabase (`editDevice` deviceUpdate)
                `rescue` textError internalServerError500
        case updateReturn of
            Left e -> textError internalServerError500 e
            Right _ -> redirect "/editDevices"

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

