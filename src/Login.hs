module Login
(
    Login,
    newLogin,
    loginAttempt,
    logout
)
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Set


-- ^ Public data and functions

data Login = Login { requestChan :: TChan Request };

newLogin :: IO Login
newLogin = do
    chan <- atomically newTChan
    void $ forkIO $ loginHandler chan
    return $ Login chan

loginAttempt :: Login -> String -> IO Bool
loginAttempt this name = do
    chan <- atomically newTChan
    atomically $ writeTChan (requestChan this) $ LoginReq name chan
    atomically $ readTChan chan

logout :: Login -> String -> IO()
logout this name = atomically $ writeTChan (requestChan this) $ LogoutReq name


-- ^ Private data and functions

data Request = LoginReq String (TChan Bool) | LogoutReq String

loginHandler :: TChan Request -> IO ()
loginHandler chan = loop empty where
    loop names = do
        request <- atomically $ readTChan chan
        case request of
            LogoutReq name -> loop (delete name names)
            LoginReq name answerChan -> do
                atomically $ writeTChan answerChan (notMember name names)
                loop (insert name names)


