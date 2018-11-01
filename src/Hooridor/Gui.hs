module Hooridor.Gui where
import Hooridor.Core
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
--import Graphics.Gloss.Interface.Pure.Color

type Board = [(Cell,Color)]

data GuiState = GuiState GameState Board

data CellOrWall = Cell' Cell | Wall' Wall deriving (Show)

build :: (Int,Int) -> (Float, Float)
-- TODO: Generalize for size
build (x,y) = ((fromIntegral (x-4)*50),(fromIntegral (y-4)*50))

inverseBuild :: (Float, Float) -> (Int,Int)
--Same as above
inverseBuild (x,y) = (((round (x/50))+4),((round (y/50))+4))

inverseBuild' :: (Float, Float) -> Maybe CellOrWall
inverseBuild' (x, y)
  | row < 0 || row > 8 || col < 0 || row > 8 = Nothing
  | not addRow && not addCol = Just (Cell' (row, col))
  | addRow && not addCol = Just (Wall'
                                  (((row, col), (row + 1, col))
                                  , ((row, col + 1), (row + 1, col + 1))))
  | not addRow && addCol = Just (Wall'
                                  (((row, col), (row, col + 1))
                                  , ((row + 1, col), (row + 1, col + 1))))
  | addRow && addCol = Nothing
  where
    x' = round x + 420 :: Int
    y' = round y + 420 :: Int
    (xDiv, xMod) = divMod x' 50
    (yDiv, yMod) = divMod y' 50
    row = yDiv 
    addRow = yMod > 40
    col = xDiv
    addCol = xMod > 40


colorCell :: (Cell,Color) -> Board
-- Same as above 
colorCell ((x',y'),c) = [((x,y),(chooseColor x y)) 
                            | x<-[0..8], y<-[0..8]]
                                where
                                    chooseColor x y 
                                            | x==x' && y==y' = c
                                            | otherwise = black

-- | Respond to key events.
handleEvents :: Event -> GuiState -> GuiState
handleEvents (EventKey (MouseButton _) Down _ (x', y')) gs 
                    = GuiState (tryMove (MakeMove (x,y)) gameState) board
                    where 
                        (GuiState gameState board) = gs
                        (x,y) = inverseBuild (x',y')
handleEvents (EventMotion (x',y')) gs = GuiState (gameState) (colorCell ((x,y),red))
                    where 
                        (GuiState gameState _) = gs
                        (x,y) = inverseBuild (x',y')
handleEvents _ z = z

window :: Display
window = InWindow "Hooridor? A?" (600, 600) (0, 0)

background :: Color
background = white

fps :: Int
fps = 60

colorPlayer :: PlayerColor -> Color
colorPlayer Red = red
colorPlayer Green = green
colorPlayer Yellow = yellow
colorPlayer Orange = orange

drawPlayer :: Player -> Picture
drawPlayer p = translate x y (color (colorPlayer (pcolor p)) (circleSolid 15))
                where 
                    (xr,yr) = pos p
                    (x,y) = build (xr,yr)

renderCell :: (Cell,Color) -> Picture
renderCell ((x,y),c) = translate x' y' 
                    ((color c (rectangleSolid 40 40)))
                    where 
                        (x',y') = build (x,y)

drawBoard :: Board -> Picture
drawBoard board = (pictures 
            (map renderCell board)) 
             
                                
render :: Int -> GuiState -> Picture
render size (GuiState gameState board) = translate x y (drawBoard board <>
                pictures (map drawPlayer ( players)))
                    where 
                        (x,y) = (0,0) --build (-size,-size)
                        players = playerList (gameState)
                        
-- Create new GuiState with board of given size                        
initiateGame :: Int -> Int -> GuiState
initiateGame pc size = GuiState (initialState pc) board
                        where 
                            board = [ ((x,y),black) | x<-[0..size], y<-[0..size]]


update :: Float -> GuiState -> GuiState
update _ = id

playGame :: Int-> Int -> IO ()
playGame size pc = play window background fps (initiateGame pc size) 
            (render size) handleEvents update
