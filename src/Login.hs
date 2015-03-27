module Login where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Set


-- ^ Public data and functions

data Login = Login { requestChan :: TChan Request };
data Request = LoginReq String (TChan Bool) | LogoutReq String

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

loginHandler :: TChan Request -> IO ()
loginHandler chan = loop empty where
    loop names = undefined


