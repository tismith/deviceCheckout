{-# LANGUAGE OverloadedStrings #-}

--Web and scotty
import Web.Scotty.Trans (scottyT)

--Monads
import Control.Monad.Reader (runReaderT)

--System
import System.Exit (exitSuccess)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

--Concurrency
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
    takeMVar, putMVar)

--Option parsing
import Options.Applicative (execParser)

--Local imports
import Types (ApplicationOptions(..), Application(..))
import CmdLine (applicationOptionsParser)
import Routes (routes)

handler :: MVar () -> IO ()
handler mutex = putMVar mutex ()

main :: IO ()
main = do
    options <- execParser applicationOptionsParser
    let application = Application options (newMVar ())

    --start scotty in a background thread, so we can exit on a signal
    _ <- forkIO $
        scottyT (portNumber options) (`runReaderT` application) routes

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
