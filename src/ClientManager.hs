module ClientManager (
    ClientServer,
    newServer,
    closeServer,
    addClient,
    removeClient,
    tell,
    broadcast
)
where


import Data.Map
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import AutoCloseable
import ClientConnection


-- ^ Public methods

data ClientServer = ClientServer { serverChan :: TChan InputMessage };

newServer :: IO ClientServer
newServer = do
    chan <- atomically newTChan
    let server = ClientServer chan
    _ <- forkIO $ worker server
    return server

closeServer :: ClientServer -> STM() 
closeServer server = writeTChan (serverChan server) ServerShut

instance AutoCloseable ClientServer where
    tryWith = bracket newServer (atomically . closeServer)

instance IManager ClientServer where
    addClient server client      = writeTChan (serverChan server) (Hello client)
    removeClient server name     = writeTChan (serverChan server) (Close name)
    tell server src dst content  = writeTChan (serverChan server) (Tell src content dst)
    broadcast server src content = writeTChan (serverChan server) (BroadCast src content)


-- ^ Private methods

data InputMessage
    = Hello     { client :: ClientConnection }
    | Tell      { from :: String, text :: String, to :: String }
    | BroadCast { from :: String, text :: String }
    | Close     { from :: String }
    | ServerShut

data ServerState = ServerState { clients :: Map String ClientConnection };

worker :: ClientServer -> IO()
worker server = loop (ServerState empty) where
    loop state = do
        msg <- atomically $ readTChan (serverChan server)
        (continue, newState) <- atomically $ handleInput state msg
        when continue $ loop newState


handleInput :: ServerState -> InputMessage -> STM (Bool, ServerState)

handleInput state Hello{..} = do
    let newState = ServerState { clients = insert (clientName client) client (clients state) }
    return (True, newState)

handleInput state Close{..} = do
    let newState = ServerState { clients = delete from (clients state) }
    return (True, newState)

handleInput state Tell{..} = do
    let found = Data.Map.lookup to (clients state)
    when (isJust found) $ sendToClient (fromJust found) from text 
    return (True, state)

handleInput state BroadCast{..} = do
    mapM_ (\c -> sendToClient c from text) $ elems (clients state)
    return (True, state)

handleInput state ServerShut = do
    mapM_ closeClient $ elems (clients state)
    return (False, state)

