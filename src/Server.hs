module Server (runServer) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Network
import System.IO

import AutoCloseable
import ClientConnection
import ChatManager
import Login


-- ^ Public data and function

runServer :: Int -> IO()
runServer port = withSocketsDo $ withListenOn port $
    \socketServer -> do
        putStrLn $ "Listening on port: " ++ show port
        race_ (tryWith $ serverLoop socketServer) inputLoop


-- ^ Private data and function

inputLoop :: IO ()
inputLoop = do
    -- TODO - to send command to the server. Does not work
    cmd <- getLine
    unless ("exit" == cmd) inputLoop

serverLoop :: Socket -> ChatManager -> IO()
serverLoop socketServer clientManager = do
    login <- newLogin
    forever $ do
        (h, _, _) <- accept socketServer
        hSetBuffering h LineBuffering
        putStrLn "Connection accepted..."
        newClient login clientManager h


withListenOn :: Int -> (Socket -> IO a) -> IO a
withListenOn port = bracket
    (listenOn (PortNumber (fromIntegral port))) sClose

