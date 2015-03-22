module ClientConnection
(
    ClientConnection(clientName),
    IManager(..),
    newClient,
    closeClient,
    sendToClient
)
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import System.IO


-- ^ Public methods

data OutputMessage = Message { from :: String, text :: String} | Shut

data ClientConnection = ClientConnection {
    clientName  :: String,
    inputChan   :: TChan OutputMessage
};

class IManager server where
    addClient ::    server -> ClientConnection -> STM()
    removeClient :: server -> String -> STM()
    tell ::         server -> String -> String -> String -> STM()
    broadcast ::    server -> String -> String -> STM()


newClient :: (IManager m) => m -> Handle -> IO()
newClient server handle = do
    _ <- forkFinally (clientSetup server handle) (\_ -> hClose handle)
    return ()

closeClient :: ClientConnection -> STM()
closeClient client = writeTChan (inputChan client) Shut

sendToClient :: ClientConnection -> String -> String -> STM()
sendToClient client src text = writeTChan (inputChan client) (Message src text)


-- ^ Private methods

clientSetup :: (IManager server) => server -> Handle -> IO ()
clientSetup server socket = do
    msg <- hGetLine socket
    when ("/hello " `isPrefixOf` msg) $ do
        chan <- atomically newTChan
        let clientName = init $ drop (length "/hello ") msg
        let connection = ClientConnection clientName chan
        atomically $ addClient server connection
        clientLoop connection server socket


clientLoop :: (IManager server) => ClientConnection -> server -> Handle -> IO ()
clientLoop this server socket = void $ race sendLoop receiveLoop where
    chan = inputChan this
    receiveLoop = do
        msg <- hGetLine socket --TODO handle exception ? socket broken suddenly?
        continue <- handleMessage (clientName this) server msg
        when continue receiveLoop
    sendLoop = do
        msg <- atomically $ readTChan chan
        continue <- sendMessage socket msg
        when continue sendLoop


sendMessage :: Handle -> OutputMessage -> IO Bool
sendMessage socket Message{..} = do
    let message = "/message " ++ from ++ " " ++ text
    hPutStrLn socket message
    return True
sendMessage socket Shut = do
    hPutStrLn socket "/shut"
    return False


handleMessage :: (IManager server) => String -> server -> String -> IO Bool
handleMessage src server msg

    | "/tell " `isPrefixOf` msg = do
        let tellMsg = init $ drop (length "/tell ") msg
        let spaceIdx = elemIndex ' ' tellMsg
        when (isJust spaceIdx) $ do
            let (dst, txt) = splitAt (fromJust spaceIdx) tellMsg
            atomically $ tell server src dst txt
        return True
    
    | "/broadcast " `isPrefixOf` msg = do
        let txt = init $ drop (length "/broadcast ") msg
        atomically $ broadcast server src txt
        return True
    
    | "/shut" `isPrefixOf` msg = do
        atomically $ removeClient server src
        return False
    
    | otherwise = do 
        print ("Invalid message received " ++ msg ++ "\n")
        return True
