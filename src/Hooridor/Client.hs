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

data Message = Message {userId:: Int, message:: GameState} deriving (Read)

type ActionChannel = TChan Turn

type GameUniverse = TVar GuiState

type ClientId = TVar Int

data ClientState = ClientState GameUniverse ClientId

-- | Wrap game event handler with writing ations to channels
-- | Also needed because Client has different state than Gui
withHandler
  :: ActionChannel
  -> (Event -> GuiState -> GuiState)
  -> Event
  -> ClientState
  -> IO ClientState
withHandler channel f evt@(EventKey (MouseButton _) Down _ (x', y')) c@(ClientState universe cidVar) = do
  cid <- readTVarIO cidVar
  gs@(GuiState gameState _) <- readTVarIO universe
  if samePlayer (last (take cid players)) (currentPlayer gameState) then
      case pointingAt (x', y') of
        Just (BoardCell a) -> do
          atomically $ writeTChan channel (MakeMove a)
          atomically $ writeTVar universe (f evt gs)
        Just (BoardWall w) -> do
          atomically $ writeTChan channel (PutWall w)
          atomically $ writeTVar universe (f evt gs)
        Nothing ->  atomically $ writeTVar universe (f evt gs)
    else
      atomically $ writeTVar universe gs
  return c

-- | Show placeholder only if client is allowed
withHandler _ f evt@(EventMotion _) c@(ClientState universe cidVar) = do
  cid <- readTVarIO cidVar
  gs@(GuiState gameState _) <- readTVarIO universe
  if samePlayer (last (take cid players)) (currentPlayer gameState) then
    atomically $ writeTVar universe (f evt gs) else
    atomically $ writeTVar universe gs
  return c

withHandler _ f evt c@(ClientState universe _) = do
  gs <- readTVarIO universe
  atomically $ writeTVar universe (f evt gs)
  return c

updateClient :: Float -> ClientState -> IO ClientState
updateClient _dt = return

-- | Render universe from CLient state using Gui renderer
withRender :: (GuiState -> Picture) -> ClientState -> IO Picture
withRender f (ClientState universe _) = do
  gs <- readTVarIO universe
  return (f gs)

-- | Receive game state from server and write into game universe
consume :: Socket -> ClientId -> GameUniverse -> IO ()
consume sock cid channel = do
   msg <- try (recv sock 1024) :: IO (Either IOException B.ByteString)
   case msg of
        Left error -> do
          pure ()
        Right message -> do
          let (Message uid gs) = (read (BS.unpack message))
          atomically $ writeTVar cid uid
          atomically $ modifyTVar channel (\(GuiState _ b) -> (GuiState gs b))
          consume sock cid channel

-- | Read from channel of actions and send them to server
produce :: Socket -> ActionChannel -> IO ()
produce sock channel = forever $ do
  action <- atomically $ readTChan channel
  sendAll sock (BS.pack (show action))
  pure ()

-- | Create 2 processes to send and receive messages from/to server
acceptSocket :: Socket -> ClientId -> GameUniverse -> ActionChannel -> IO ()
acceptSocket sock cid stateVar actionChannel = do
  _ <- forkIO $ produce sock actionChannel
  _ <- forkIO $ consume sock cid stateVar
  return ()

-- | Connect to server
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
  universe <- newTVarIO $ initiateGame 4
  uid <- newTVarIO 0
  let init = ClientState universe uid
  acceptSocket sock uid universe actionChannel
  playIO window background fps
    init
    (withRender render)
    (withHandler actionChannel handleEvents)
    updateClient
