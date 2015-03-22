module Server (runServer) where

import Control.Exception
import Control.Monad
import Network
import System.IO

import AutoCloseable
import ClientConnection
import ClientManager


-- ^ Public methods

runServer :: Int -> IO()
runServer port = withSocketsDo $ withListenOn port $
    \socketServer -> do
        putStrLn $ "Listening on port: " ++ show port
        tryWith $ serverLoop socketServer


-- ^ Private methods

serverLoop :: Socket -> ClientServer -> IO()
serverLoop socketServer clientManager =
    forever $ do
        (h, _, _) <- accept socketServer
        hSetBuffering h LineBuffering
        newClient clientManager h


withListenOn :: Int -> (Socket -> IO a) -> IO a
withListenOn port = bracket
    (listenOn (PortNumber (fromIntegral port))) sClose

