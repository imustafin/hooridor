module Hooridor.Gui where
import Hooridor.Core
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


-- | Respond to key events.
handleEvents :: Event -> GameState -> GameState
handleEvents _ = id

window :: Display
window = InWindow "Hooridor? A?" (600, 600) (0, 0)

background :: Color
background = white

fps :: Int
fps = 60

render :: GameState -> Picture
render _ = blank

update :: Float -> GameState -> GameState
update _ = id

playGame :: Int -> IO ()
playGame pc = play window background fps (initialState pc) render handleEvents update
