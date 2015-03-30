module Main (main) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Network
import System.Environment
import System.IO

import AutoCloseable
import ClientConnection
import ChatManager
import Login


main :: IO()
main = do
    [portStr] <- getArgs
    let port = read portStr :: Int
    runServer port

    
runServer :: Int -> IO()
runServer port = withSocketsDo $ withListenOn port $
    \socketServer -> do
        putStrLn $ "Listening on port: " ++ show port
        race_ (tryWith $ serverLoop socketServer) inputLoop


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
    