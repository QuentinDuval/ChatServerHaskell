module Server (runServer) where

import Control.Exception
import Control.Monad
import Network
import System.IO

import AutoCloseable
import ClientConnection
import ClientManager
import Login


-- ^ Public data and function

runServer :: Int -> IO()
runServer port = withSocketsDo $ withListenOn port $
    \socketServer -> do
        putStrLn $ "Listening on port: " ++ show port
        tryWith $ serverLoop socketServer


-- ^ Private data and function

serverLoop :: Socket -> ClientServer -> IO()
serverLoop socketServer clientManager = do
    login <- newLogin
    forever $ do
        (h, _, _) <- accept socketServer
        hSetBuffering h LineBuffering
        newClient login clientManager h


withListenOn :: Int -> (Socket -> IO a) -> IO a
withListenOn port = bracket
    (listenOn (PortNumber (fromIntegral port))) sClose

