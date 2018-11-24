{-# LANGUAGE PatternSynonyms #-}

module Hooridor.Gui.GameModeMenu
  ( withGameModeMenu
  , GMMenuResult (LocalGame, EasyAi, HardAi)
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Hooridor.Gui.Types

-- |Game mode menu state.
data GMMenu a = GMMenu GMMenuResult (Maybe a)

-- |Options which can be selected in this menu.
data GMMenuResult
  = LocalGame Int -- ^No AI players, no networking, 2 to 4 players
  | EasyAi -- ^Easy AI player
  | HardAi -- ^Hard AI player
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
next HardAi = HardAi

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

-- |Draw menu if there is no game, or draw game.
render :: Renderer a -> Renderer (GMMenu a)
render aRenderer (GMMenu _ (Just aState)) = aRenderer aState
render _ (GMMenu result Nothing)
  = scale 0.25 0.25 (((translate (-250) 0) . text . show) result)

-- |Handle menu events if there is no game, or pass event to game.
handle :: (GMMenuResult -> a) -> Handler a -> Handler (GMMenu a)
handle _ _ PressR (GMMenu _ _) = initial
handle _ _ PressUp (GMMenu result Nothing) = GMMenu (next result) Nothing
handle _ _ PressDown (GMMenu result Nothing) = GMMenu (prev result) Nothing
handle makeInitial _ PressEnter (GMMenu result Nothing)
  = GMMenu result ((Just . makeInitial) result)
handle _ _ _ state@(GMMenu _ Nothing) = state
handle _ handler event (GMMenu result (Just aState))
  = GMMenu result ((Just . handler event) aState)

-- |Handle per-time updates for menu (no updates) if there is no game,
-- or pass time update to game.
update :: Updater a -> Updater (GMMenu a)
update _ _ state@(GMMenu _ Nothing) = state
update updater dt (GMMenu result (Just aState))
  = GMMenu result ((Just . updater dt) aState)

-- |Add AI selection menu to a game.
withGameModeMenu
  :: (GMMenuResult -> a) -- ^Make initial game state with this AI level
  -> PlayType (GMMenu a) -- ^'play' function
  -> Display -- ^Display to draw on
  -> Color -- ^Background color
  -> Int -- ^FPS count
  -> (Renderer a) -- ^Rendering function for the game
  -> (Handler a) -- ^Event handling function for the game
  -> (Updater a) -- ^Per-time updating function for the game
  -> IO ()
withGameModeMenu
  makeInitial play' window' background' fps' renderer handler updater
  = play' window' background' fps' initial
    (render renderer) (handle makeInitial handler) (update updater)
