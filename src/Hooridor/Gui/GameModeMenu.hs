{-# LANGUAGE PatternSynonyms #-}

module Hooridor.Gui.GameModeMenu
  ( withGameModeMenu
  , GMMenuResult (LocalGame, EasyAi, HardAi, Network)
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Hooridor.Gui.Types

-- |Game state with game.
data StateWithGame a = StateWithGame a (Game a)

-- |IO game state with IO game.
data StateWithGameIO a = StateWithGameIO a (GameIO a)

-- |Game mode menu state.
data GMMenu a = GMMenu GMMenuResult (Maybe (StateWithGame a))

-- |IO game mode menu state.
data GMMenuIO a = GMMenuIO GMMenuResult (Maybe (StateWithGameIO a))


-- |Options which can be selected in this menu.
data GMMenuResult
  = LocalGame Int -- ^No AI players, no networking, 2 to 4 players
  | EasyAi -- ^Easy AI player
  | HardAi -- ^Hard AI player
  | Network -- ^Connect to a server for online play
  deriving (Show)

-- |Menu option after the given option.
--
-- If there are no more options, returns the same option:
-- >>> next Network = Network
next :: GMMenuResult -> GMMenuResult
next (LocalGame pc)
  | pc < 4 = LocalGame (pc + 1)
  | otherwise = EasyAi
next EasyAi = HardAi
next HardAi = Network
next Network = Network

-- |Menu option before the given option.
--
-- If there are no options before this, returns the same option:
-- >>> prev NoAi = NoAi
prev :: GMMenuResult -> GMMenuResult
prev (LocalGame pc)
  | pc > 2 = LocalGame (pc - 1)
  | otherwise = LocalGame 2
prev EasyAi = LocalGame 4
prev HardAi = EasyAi
prev Network = HardAi

-- |Pattern for pressing key Up.
pattern PressUp :: Event
pattern PressUp <- EventKey (SpecialKey KeyUp) Down _ _

-- |Pattern for pressing key Down.
pattern PressDown :: Event
pattern PressDown <- EventKey (SpecialKey KeyDown) Down _ _

-- |Pattern for pressing key Enter.
pattern PressEnter :: Event
pattern PressEnter <- EventKey (SpecialKey KeyEnter) Down _ _

-- |Pattern for pressing key R.
pattern PressR :: Event
pattern PressR <- EventKey (Char 'r') Down _ _

-- |Initial menu state.
initial :: GMMenu a
initial = GMMenu (LocalGame 2) Nothing

initialIO :: GMMenuIO a
initialIO = GMMenuIO (LocalGame 2) Nothing

-- |Draw menu if there is no game, or draw game.
render :: Renderer (GMMenu a)
render (GMMenu result Nothing)
  = scale 0.25 0.25 (((translate (-250) 0) . text . show) result)
render (GMMenu _ (Just stateWithGame)) = renderer state
  where
    (StateWithGame state (Game  _ renderer _ _)) = stateWithGame

-- |Draw menu if there is no game, or draw IO game.
renderIO :: RendererIO (GMMenuIO a)
renderIO (GMMenuIO result Nothing)
  = do return (render (GMMenu result Nothing))
renderIO (GMMenuIO _ (Just (StateWithGameIO state (GameIO _ rendererIO _ _))))
  = rendererIO state

-- |Handle menu events if there is no game, or pass event to game.
-- Game creation happens here when menu option is selected.
handle :: (GMMenuResult -> Game a) -> Handler (GMMenu a)
handle _ PressR (GMMenu result _) = GMMenu result Nothing
handle _ PressUp (GMMenu result Nothing) = GMMenu (next result) Nothing
handle _ PressDown (GMMenu result Nothing) = GMMenu (prev result) Nothing
handle makeGame PressEnter (GMMenu result Nothing)
  = GMMenu result (Just (StateWithGame initial' game))
  where
    game@(Game initial' _ _ _) = makeGame result
handle _ _ state@(GMMenu _ Nothing) = state
handle _ event (GMMenu result (Just stateWithGame))
  = GMMenu result (Just (StateWithGame ((handler event) state) game))
  where
    (StateWithGame state game@(Game _ _ handler _)) = stateWithGame

handleIO :: (GMMenuResult -> IO (GameIO a)) -> Handler (GMMenuIO a)
handleIO makeGameIO PressEnter (GMMenuIO result Nothing)
  = GMMenuIO result (Just (StateWithGameIO initial' game))
  where
    game@(GameIO initial' _ _ _) = do return (makeGameIO result)

-- |Handle per-time updates for menu (no updates) if there is no game,
-- or pass time update to game.
update :: Updater (GMMenu a)
update _ menuState@(GMMenu _ Nothing) = menuState
update dt (GMMenu result (Just (StateWithGame state game@(Game _ _ _ updater))))
  = GMMenu result (Just (StateWithGame (updater dt state) game))

-- |Add a game mode selection menu.
withGameModeMenu
  :: PlayType (GMMenu a) -- ^'play' function.
  -> Display -- ^Display to draw on.
  -> Color -- ^Background color.
  -> Int -- ^FPS count.
  -> (GMMenuResult -> Game a) -- ^Make game from result.
  -> IO ()
withGameModeMenu play' window background fps makeGame
  = play' window background fps initial render (handle makeGame) update

withGameModeMenuIO
  :: PlayTypeIO (GMMenuIO a) -- ^'playIO' function.
  -> Display -- ^Display to draw on.
  -> Color -- ^Background color.
  -> Int -- ^FPS count.
  -> (GMMenuResult -> GameIO a) -- ^Make IO game from result.
  -> IO ()
withGameModeMenuIO
  playIO' window background fps makeGameIO
  = playIO' window background fps initialIO renderIO (handleIO makeGameIO) updateIO
