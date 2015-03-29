module ClientConnection
(
    ClientConnection(clientName),
    IManager(..),
    newClient,
    closeClient,
    sendToClient,
    notifyNewConnection,
    notifyDisconnection
)
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Login


-- ^ Public data and function

data ClientConnection = ClientConnection {
    clientName  :: String,
    inputChan   :: TChan OutputMessage
};

class IManager server where
    addClient ::    server -> ClientConnection -> STM()
    removeClient :: server -> String -> STM()
    tell ::         server -> String -> String -> String -> STM()
    broadcast ::    server -> String -> String -> STM()


newClient :: (IManager m) => Login -> m -> Handle -> IO()
newClient login server h = void $ forkFinally (clientSetup login server h) (\_ -> hClose h)

closeClient :: ClientConnection -> IO()
closeClient client = atomically $ writeTChan (inputChan client) Shut

sendToClient :: ClientConnection -> String -> String -> IO()
sendToClient client src text = atomically $ writeTChan (inputChan client) (Message src text)

notifyNewConnection :: String -> ClientConnection -> IO()
notifyNewConnection src client = atomically $ writeTChan (inputChan client) (NewConnection src)

notifyDisconnection :: String -> ClientConnection -> IO()
notifyDisconnection src client = atomically $ writeTChan (inputChan client) (Disconnection src)


-- ^ Private data and function

data OutputMessage
    = Message { from :: String, text :: String}
    | NewConnection { from :: String }
    | Disconnection { from :: String }
    | Shut


clientSetup :: (IManager server) => Login -> server -> Handle -> IO ()
clientSetup login server socket = do
    msg <- hGetLine socket --TODO handle exception of socket broken suddenly
    when ("/hello " `isPrefixOf` msg) $ do
        let clientName = init $ drop (length "/hello ") msg
        successfulLogin <- loginAttempt login clientName
        if successfulLogin
            then clientLoopInit login server socket clientName
            else void $ sendMessage socket Shut


clientLoopInit :: (IManager server) => Login -> server -> Handle -> String -> IO()
clientLoopInit login server socket clientName = do
    chan <- atomically newTChan
    let connection = ClientConnection clientName chan
    bracket_
        (atomically $ addClient server connection)
        (do atomically $ removeClient server clientName; logout login clientName)
        (clientLoop connection server socket)


clientLoop :: (IManager server) => ClientConnection -> server -> Handle -> IO ()
clientLoop this server socket = void $ race sendLoop receiveLoop where
    chan = inputChan this
    receiveLoop = do
        msg <- hGetLine socket --TODO handle exception ? socket broken suddenly
        !continue <- handleMessage (clientName this) server msg
        when continue receiveLoop
    sendLoop = do
        msg <- atomically $ readTChan chan
        !continue <- sendMessage socket msg
        when continue sendLoop


sendMessage :: Handle -> OutputMessage -> IO Bool
sendMessage h Message{..} =       hPutStrLn h ("/message " ++ from ++ " " ++ text) >> return True
sendMessage h NewConnection{..} = hPutStrLn h ("/newconnection " ++ from) >> return False
sendMessage h Disconnection{..} = hPutStrLn h ("/disconnection " ++ from) >> return False
sendMessage h Shut =              hPutStrLn h "/shut" >> return False


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
    
    | "/shut" `isPrefixOf` msg = return False
    | otherwise = return True
