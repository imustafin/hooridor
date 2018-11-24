{-# LANGUAGE RecordWildCards #-}

module Hooridor.Server where

import Hooridor.Core
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

data Message = Message {userId:: Int, message:: GameState} deriving (Show)

data SendMessage = SendMessage {senderId:: Int, turn:: Turn}

type GameChannel = TChan Message
type TurnChannel = TChan SendMessage

type Config = TVar GameState


-- | Receive a turn move from a player
consume :: Int -> Socket -> GameChannel -> Config -> IO ()
consume player sock channel config = do
    msg <- try (recv sock 1024) :: IO (Either IOException B.ByteString)
    case msg of
        Left _ -> pure()
        Right message -> do
          currentState <- readTVarIO config
          if samePlayer (last (take player players)) (currentPlayer currentState) then
            atomically $ modifyTVar config (takeTurn (read (BS.unpack message))) else
            atomically $ writeTVar config currentState
          newState <- readTVarIO config
          atomically $ writeTChan channel $ Message player newState
          consume player sock channel config

-- | Send current game state to all active players
produce :: Int -> Socket -> GameChannel -> IO ()
produce player sock channel = forever $ do
    (Message _ msg) <- atomically $ readTChan channel
    _ <- send sock (BS.pack (show (Message player msg)))
    pure ()

-- | Accept a socket connection for a player
-- | Create one process for cunsuming messages form player and another to broadcast state
-- | On connect bradcasts to all players a new state
-- | Does not respond if more than 4 players are connected
acceptSocket :: Int -> Socket -> GameChannel -> Config -> IO ()
acceptSocket n s gc c | n > 4 = acceptSocket n s gc c
acceptSocket player listeningSocket channel config = do
    sock <- fst <$> accept listeningSocket
    broadcastChannel <- atomically $ dupTChan channel

    _ <- forkIO $ consume player sock broadcastChannel config

    _ <- forkIO $ produce player sock broadcastChannel

    atomically $ writeTVar config (initialState player)
    atomically $ writeTChan broadcastChannel $ Message player (initialState player)

    acceptSocket (player + 1) listeningSocket channel config

-- | Create a network server
createSocket :: PortNumber -> IO Socket
createSocket port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    pure sock

startServer :: PortNumber -> IO ()
startServer port = do
    broadcastChannel <- atomically newBroadcastTChan :: IO GameChannel
    sock <- createSocket port
    config <- newTVarIO (initialState 0)
    acceptSocket 1 sock broadcastChannel config
