module Hooridor.Client where

import Hooridor.Core
import Hooridor.Gui
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

type ActionChannel = TChan Turn

type GameUniverse = TVar GuiState

data ClientState = ClientState GameUniverse

withHandler
  :: ActionChannel
  -> (Event -> GuiState -> GuiState)
  -> Event
  -> ClientState
  -> IO ClientState
withHandler channel f evt@(EventKey (MouseButton _) Down _ (x', y')) c@(ClientState universe) = do
  gs <- readTVarIO universe
  case inverseBuild' (x',y') of
    Just b ->
      case b of
        Cell' a -> do
          atomically $ writeTChan channel (MakeMove a)
        Wall' a -> do
          atomically $ writeTChan channel (PutWall a)
  atomically $ writeTVar universe (f evt gs)
  return c
withHandler _ f evt c@(ClientState universe) = do
  gs <- readTVarIO universe
  atomically $ writeTVar universe (f evt gs)
  return c

updateClient :: Float -> ClientState -> IO ClientState
updateClient _dt cs = return cs

withRender :: (GuiState -> Picture) -> ClientState -> IO Picture
withRender f (ClientState universe) = do
  gs <- readTVarIO universe
  return (f gs)

consume :: Socket -> GameUniverse -> IO ()
consume sock channel = do
   msg <- try (recv sock 1024) :: IO (Either IOException B.ByteString)
   case msg of
        Left error -> do
          pure ()
        Right message -> do
          atomically $ modifyTVar channel (\(GuiState _ b) ->
                                            (GuiState (read (BS.unpack message)) b))
          consume sock channel

produce :: Socket -> ActionChannel -> IO ()
produce sock channel = forever $ do
  action <- atomically $ readTChan channel
  sendAll sock (BS.pack (show action))
  pure ()

acceptSocket :: Socket -> GameUniverse -> ActionChannel -> IO ()
acceptSocket sock stateVar actionChannel = do
  _ <- forkIO $ produce sock actionChannel
  _ <- forkIO $ consume sock stateVar
  return ()

createSocket :: String -> String -> IO Socket
createSocket host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock <- socket AF_INET Stream 0
  setSocketOption sock NoDelay 1
  connect sock $ addrAddress addr
  return sock

startClient :: String -> String -> IO ()
startClient host port =  withSocketsDo $ do
  actionChannel <- atomically newTChan :: IO ActionChannel
  sock <- createSocket host port
  universe <- newTVarIO $ initiateGame 4 8
  let init = ClientState universe
  acceptSocket sock universe actionChannel
  playIO window background fps
    init
    (withRender (render 8))
    (withHandler actionChannel handleEvents)
    updateClient
