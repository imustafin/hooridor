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


data Message = Message {userId:: Int, message:: GameState}

data SendMessage = SendMessage {senderId:: Int, turn:: Turn}

type GameChannel = TChan Message
type TurnChannel = TChan SendMessage

type Config = TVar GameState

consume :: Int -> Socket -> GameChannel -> Config -> IO ()
consume userId sock channel config = do
    msg <- try (recv sock 1024) :: IO (Either IOException B.ByteString)
    case msg of
        Left error -> do
          pure ()
        Right message -> do
          atomically $ modifyTVar config (\ c -> (takeTurn (read (BS.unpack message)) c))
          newState <- atomically $ readTVar config
          atomically $ writeTChan channel $ Message userId newState
          consume userId sock channel config

produce :: Int -> Socket -> GameChannel -> IO ()
produce userId sock channel = forever $ do
    (Message senderId msg) <- atomically $ readTChan channel
    send sock (BS.pack (show msg))
    pure ()

acceptSocket :: Int -> Socket -> GameChannel -> Config -> IO ()
acceptSocket n _ _ _ | n > 4 = pure ()
acceptSocket userId listeningSocket channel config = do
    sock <- fst <$> accept listeningSocket
    broadcastChannel <- atomically $ dupTChan channel

    _ <- forkIO $ consume userId sock broadcastChannel config

    _ <- forkIO $ produce userId sock broadcastChannel

    atomically $ writeTVar config (initialState userId)
    atomically $ writeTChan broadcastChannel $ Message userId (initialState userId)

    acceptSocket (userId + 1) listeningSocket channel config

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
