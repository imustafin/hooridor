module Hooridor.Gui where

import Hooridor.Core
  (GameState
  , Cell
  , Wall
  , WallPart
  , PlayerColor (Red, Green, Yellow, Orange)
  , Player
  , tryMove
  , Turn (MakeMove, PutWall)
  , pcolor
  , currentPlayer
  , pos
  , walls
  , isWinner
  , playerList
  , initialState
  )
import Hooridor.Ai
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List

--import Graphics.Gloss.Interface.Pure.Color

type Board = Cell -> Color

defaultCellColor :: Color
defaultCellColor = black

normalWallColor :: Color
normalWallColor = black

hintWallColor :: Color
hintWallColor = greyN 0.5

-- |State of the GUI: current GameState and additional decoration to draw
data GuiState = GuiState GameState Picture

data BoardObject = BoardCell Cell | BoardWall Wall
  deriving (Show)

-- |Center of the Cell drawn on the screen.
cellCenter :: Cell -> Point
cellCenter (x,y) = ((fromIntegral (x-4)*50),(fromIntegral (y-4)*50))

-- |What board object is under the mouse cursor.
pointingAt :: Point -> Maybe BoardObject
pointingAt (x, y)
  | row < 0 || row > 8 || col < 0 || col > 8 = Nothing
  | not higherThanCell && not righterThanCell
      = Just (BoardCell (row, col))
  | higherThanCell && not righterThanCell
      = Just (BoardWall
               ( ((row, col),     (row + 1, col))
               , ((row, col + 1), (row + 1, col + 1))))
  | not higherThanCell && righterThanCell
      = Just (BoardWall
               ( ((row, col),     (row, col + 1))
               , ((row + 1, col), (row + 1, col + 1))))
  | otherwise = Nothing -- Why we need this?
  where
    y' = round x + 220 :: Int
    x' = round y + 220 :: Int
    (xDiv, xMod) = divMod x' 50
    (yDiv, yMod) = divMod y' 50
    row = yDiv
    higherThanCell = yMod > 40 -- addrow
    col = xDiv
    righterThanCell = xMod > 40

-- |Respond to key events.
handleEvents :: Event -> GuiState -> GuiState
handleEvents (EventKey (MouseButton _) Down _ (x', y')) gs =
  case pointingAt (x', y') of
    Just (BoardCell a) -> GuiState (tryMove (MakeMove a) gameState) board
    Just (BoardWall a) -> GuiState (tryMove (PutWall a) gameState) board
    Nothing -> gs
  where
    (GuiState gameState board) = gs
handleEvents (EventMotion (x',y')) gs
  = case pointingAt (x', y') of
    Just (BoardCell a) -> GuiState (gameState) (highlightCell a c)
    Just (BoardWall w) -> GuiState (gameState) (wallHighlighting w)
    Nothing -> gs
  where
    (GuiState gameState _) = gs
    c = colorPlayer (pcolor (currentPlayer gameState))
    wallHighlighting w
      | tryMove (PutWall w) gameState /= gameState = drawWall hintWallColor w
      | otherwise = blank
handleEvents (EventKey (Char 'r') _ _ _) _ = initiateGame 2
handleEvents _ x = x

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
    (xr, yr) = pos p
    (x, y) = cellCenter (xr, yr)

drawCell :: Cell -> Color -> Picture
drawCell cell c = translate x y ((color c (rectangleSolid 40 40)))
  where
    (x, y) = cellCenter cell

drawDefaultCell :: Cell -> Picture
drawDefaultCell cell = drawCell cell defaultCellColor

highlightCell :: Cell -> Color -> Picture
highlightCell cell c = drawCell cell (dark c)

drawWallSegment :: WallPart -> Color -> Picture
drawWallSegment wp col
  | x1 == x2 = translate x y (color col (rectangleSolid 40 5))
  | otherwise = translate x y (color col (rectangleSolid 5 40))
  where
    (c1, c2) = wp
    (x1, _) = cellCenter c1
    (x2, _) = cellCenter c2
    (x, y) = segmentCoordinates wp

segmentCoordinates :: WallPart -> (Float,Float)
segmentCoordinates (c1,c2) = (x,y)
  where
    (x1,y1) = cellCenter c1
    (x2,y2) = cellCenter c2
    x = (x1+x2)/2
    y = (y1+y2)/2

drawWall :: Color  -> Wall  -> Picture
drawWall c (ws1,ws2) = (drawWallSegment ws1 c)
  <> (drawWallSegment ws2 c)
  <> translate x y (color c (rectangleSolid 10 10))
  where
    (x1,y1) = segmentCoordinates ws1
    (x2,y2) = segmentCoordinates ws2
    x = (x1+x2)/2
    y = (y1+y2)/2

drawBoard :: Picture
drawBoard = pictures cellPictures
  where
    cellPictures
      = concatMap (\x -> map (\y -> drawDefaultCell (x, y)) [0..8]) [0..8]

drawVictoryScreen :: Color -> Picture
drawVictoryScreen c = translate (-220) 0 (color (dark c) message)
  where
    message = (scale 0.5 1 (text "You have won!"))

render :: GuiState -> Picture
render (GuiState gameState decorations) =
  case winner of
    Nothing ->  translate x y (drawBoard <> decorations <>
        pictures (map drawPlayer (players)))
        <> pictures (map (drawWall normalWallColor) (walls gameState))
    Just p -> drawVictoryScreen (colorPlayer (pcolor p))
    where
      winner = find isWinner players
      (x,y) = (0,0) --build (-size,-size)
      players = playerList (gameState)

-- Create new GuiState with board of given size
initiateGame :: Int -> GuiState
initiateGame pc = GuiState (initialState pc) blank

-- Update per time
update :: Float -> GuiState -> GuiState
update _ state =
        case inteligence player of
            AI -> GuiState (aiPlayer gameState) board
            Human -> state
        where  
            (GuiState gameState board) = state
            player = currentPlayer gameState

-- Start a game on a board with this size and for this number of players
playGame :: Int -> IO ()
playGame pc
  = play window background fps
    (initiateGame pc) render
    handleEvents update
