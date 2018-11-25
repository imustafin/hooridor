module Hooridor.Gui where

import Hooridor.Gui.Game
import Hooridor.Gui.GameModeMenu
import Hooridor.Gui.Types
import Hooridor.Ai
import Hooridor.Core
import Graphics.Gloss.Interface.Pure.Game

-- |Create new GuiState with this number of players.
makeGame :: GMMenuResult -> Game GuiState
makeGame (LocalGame pc) = makeGameFromGameState (initialState pc)
makeGame EasyAi = makeGameFromGameState (initialStateAi 1)
makeGame HardAi = makeGameFromGameState (initialStateAi 2)

-- |Run the game starting with the menus.
runGui :: IO ()
runGui = withGameModeMenu play window background fps makeGame
