module Database where

import Database.SQLite.Simple (NamedParam(..),
    queryNamed, query_, execute, executeNamed, withConnection, changes)
import qualified Data.Text.Lazy as TL (Text)

import Types (Device (..), DeviceUpdate (..))

getDevices :: String -> IO [Device]
getDevices db = withConnection db $ \conn ->
                query_ conn "SELECT * FROM devices"

updateDevice :: String -> DeviceUpdate -> IO (Either TL.Text Int)
updateDevice db device =
    withConnection db $ \conn -> do
        executeNamed conn
            "UPDATE devices SET device_owner = :o, reservation_status = :r, comments = :c WHERE device_name = :n"
                [":o" := newOwner device, ":r" := newReservationStatus device,
                ":c" := newComments device, ":n" := newDeviceName device]
        numRowsChanged <- changes conn
        if numRowsChanged /= 1
            then return (Left "Database error")
            else return (Right 1)

getDevice :: String -> TL.Text -> IO (Maybe Device)
getDevice db device = withConnection db $ \conn -> do
    r <- queryNamed conn
        "SELECT * FROM devices WHERE device_name = :id" [":id" := device] :: IO [Device]
    case r of
        (b:_) -> return (Just b)
        _ -> return Nothing

deleteDevice :: String -> TL.Text -> IO (Either TL.Text Int)
deleteDevice db device = withConnection db $ \conn -> do
    executeNamed conn "DELETE FROM devices WHERE device_name = :id" [":id" := device]
    numRowsChanged <- changes conn
    if numRowsChanged /= 1
        then return (Left "Database error")
        else return (Right 1)

addDevice :: String -> Device -> IO (Either TL.Text Int)
addDevice db device = withConnection db $ \conn -> do
    execute conn "INSERT INTO devices (device_name, url, device_owner, reservation_status, comments) values (?, ?, ?, ?, ?)" device
    numRowsChanged <- changes conn
    if numRowsChanged /= 1
        then return (Left "Database error")
        else return (Right 1)

