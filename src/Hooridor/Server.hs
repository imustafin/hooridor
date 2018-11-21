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

consume :: Int -> Socket -> GameChannel -> IO ()
consume userId sock channel = do
    msg <- try (recv sock 1024) :: IO (Either IOException B.ByteString)
    case msg of
        Left error -> do
          pure ()
        Right message -> do
          atomically $ writeTChan channel $ Message userId (takeTurn
                                                            (read (BS.unpack message))
                                                            (initialState 2))
          consume userId sock channel

produce :: Int -> Socket -> GameChannel -> IO ()
produce userId sock channel = forever $ do
    (Message senderId msg) <- atomically $ readTChan channel
    send sock (BS.pack (show msg))
    pure ()

acceptSocket :: Int -> Socket -> GameChannel -> IO ()
acceptSocket n _ _ | n > 4 = return ()
acceptSocket userId listeningSocket channel = do
    sock <- fst <$> accept listeningSocket
    broadcastChannel <- atomically $ dupTChan channel

    _ <- forkIO $ consume userId sock broadcastChannel

    _ <- forkIO $ produce userId sock broadcastChannel

    acceptSocket (userId + 1) listeningSocket channel

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
    acceptSocket 1 sock broadcastChannel
