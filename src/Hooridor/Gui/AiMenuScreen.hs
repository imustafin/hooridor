{-# LANGUAGE PatternSynonyms #-}

module Hooridor.Gui.AiMenuScreen( withAiMenuScreen ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Hooridor.Gui.Types

-- |AI menu state.
data AiMenu a = AiMenu AiMenuResult (Maybe a)

-- |Options which can be selected in this menu.
data AiMenuResult
  = NoAi  -- ^No AI players
  | EasyAi -- ^Easy AI player
  | HardAi -- ^Hard AI player
  deriving (Show)

-- |Menu option after the given option.
--
-- If there are no more options, returns the same option:
-- >>> next HardAi = HardAi
next :: AiMenuResult -> AiMenuResult
next NoAi = EasyAi
next EasyAi = HardAi
next HardAi = HardAi

-- |Menu option before the given option.
--
-- If there are no options before this, returns the same option:
-- >>> prev NoAi = NoAi
prev :: AiMenuResult -> AiMenuResult
prev NoAi = NoAi
prev EasyAi = NoAi
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
initial :: AiMenu a
initial = AiMenu NoAi Nothing

-- |Draw menu if there is no game, or draw game.
render :: Renderer a -> Renderer (AiMenu a)
render aRenderer (AiMenu _ (Just aState)) = aRenderer aState
render _ (AiMenu result Nothing) = ((translate (-200) 0) . text . show) result

-- |Handle menu events if there is no game, or pass event to game.
handle :: (AiMenuResult -> a) -> Handler a -> Handler (AiMenu a)
handle _ _ PressR (AiMenu _ _) = initial
handle _ _ PressUp (AiMenu result Nothing) = AiMenu (next result) Nothing
handle _ _ PressDown (AiMenu result Nothing) = AiMenu (prev result) Nothing
handle makeInitial _ PressEnter (AiMenu result Nothing)
  = AiMenu result ((Just . makeInitial) result)
handle _ _ _ state@(AiMenu _ Nothing) = state
handle _ handler event (AiMenu result (Just aState))
  = AiMenu result ((Just . handler event) aState)

-- |Handle per-time updates for menu (no updates) if there is no game,
-- or pass time update to game.
update :: Updater a -> Updater (AiMenu a)
update _ _ state@(AiMenu _ Nothing) = state
update updater dt (AiMenu result (Just aState))
  = AiMenu result ((Just . updater dt) aState)

-- |Add AI selection menu to a game.
withAiMenuScreen
  :: (AiMenuResult -> a) -- ^Make initial game state with this AI level
  -> PlayType (AiMenu a) -- ^'play' function
  -> Display -- ^Display to draw on
  -> Color -- ^Background color
  -> Int -- ^FPS count
  -> (Renderer a) -- ^Rendering function for the game
  -> (Handler a) -- ^Event handling function for the game
  -> (Updater a) -- ^Per-time updating function for the game
  -> IO ()
withAiMenuScreen
  makeInitial play' window' background' fps' renderer handler updater
  = play' window' background' fps' initial
    (render renderer) (handle makeInitial handler) (update updater)
