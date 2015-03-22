module Server (runServer) where

import Control.Exception
import Control.Monad
import Network

import AutoCloseable
import ClientConnection
import ClientManager


runServer :: Int -> IO()
runServer port = withSocketsDo $ withListenOn port $
    \socketServer -> do
        print $ "Listening on port: " ++ show port ++ "\n"
        tryWith $ serverLoop socketServer


serverLoop :: Socket -> ClientServer -> IO()
serverLoop socketServer clientManager =
    forever $ do
        (h, _, _) <- accept socketServer
        newClient clientManager h


withListenOn :: Int -> (Socket -> IO a) -> IO a
withListenOn port = bracket
    (listenOn (PortNumber (fromIntegral port))) sClose

