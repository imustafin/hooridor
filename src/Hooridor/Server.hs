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

data Player = Player
  { playerColor :: PlayerColor
  , playerRecvChan :: TChan Turn
  , playerSendChan :: TChan GameState}

data Message = Message {userId:: Int, message:: B.ByteString}
type CicaChannel = TChan Message

consume :: Int -> Socket -> CicaChannel -> IO ()
consume userId sock channel = do
    msg <- try (recv sock 122) :: IO (Either IOException B.ByteString)
    case msg of
        Left error -> do
            atomically $ writeTChan channel $ Message 0 (BS.pack ("User [" ++ (show userId) ++ "] left\n"))
            pure ()
        Right message -> do
            atomically $ writeTChan channel $ Message userId message
            consume userId sock channel

produce :: Int -> Socket -> CicaChannel -> IO ()
produce userId sock channel = forever $ do
    (Message senderId msg) <- atomically $ readTChan channel
    if senderId == userId then
        pure ()
    else do
        send sock (BS.pack ("[" ++ (show senderId)++ "]: " ++ (BS.unpack msg)))
        pure ()


acceptSocket :: Int -> Socket ->  CicaChannel -> IO ()
acceptSocket userId listeningSocket channel = do
    sock <- fst <$> accept listeningSocket
    _ <- forkIO $ consume userId sock channel
    broadcastChannel <- atomically $ dupTChan channel
    _ <- forkIO $ produce userId sock broadcastChannel
    atomically $ writeTChan channel $ Message 0 (BS.pack ("User [" ++ (show userId) ++ "] joined\n"))
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
    broadcastChannel <- atomically newBroadcastTChan :: IO CicaChannel
    sock <- createSocket port
    acceptSocket 1 sock broadcastChannel
