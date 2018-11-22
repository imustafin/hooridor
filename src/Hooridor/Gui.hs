module Hooridor.Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
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
  , minRow
  , maxRow
  , minCol
  , maxCol
  , size
  )
import Hooridor.Ai
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--import Graphics.Gloss.Interface.Pure.Color

type Board = Cell -> Color

normalCellColor :: Color
normalCellColor = black

normalWallColor :: Color
normalWallColor = red

hintWallColor :: Color
hintWallColor = (light . light) normalWallColor

-- |State of the GUI: current GameState and additional decoration to draw
data GuiState = GuiState GameState Picture

cellSize :: Int
cellSize = 40

cellMargin :: Int
cellMargin = 10

cellSizeAndMargin :: Int
cellSizeAndMargin = cellSize + cellMargin

-- |Center of the Cell drawn on the screen.
cellCenter :: Cell -> Point
cellCenter (x,y) = ( (fromIntegral (x - halfBoard) * k)
                   , (fromIntegral (y - halfBoard) * k))
  where
    halfBoard = size `div` 2
    k = fromIntegral cellSizeAndMargin

-- |Board objects that can be pointed at by a mouse
data BoardObject = BoardCell Cell | BoardWall Wall
  deriving (Show)

-- |What board object is under the mouse cursor.
pointingAt :: Point -> Maybe BoardObject
pointingAt (x, y)
  | row < minRow || row > maxRow || col < minCol || col > maxCol = Nothing
  | not higherThanCell && not righterThanCell
      = Just (BoardCell (row, col))
  | higherThanCell && not righterThanCell
      = Just (BoardWall
               ( ((row, col),     (row + 1, col))
               , ((row, col - 1), (row + 1, col - 1))))
  | not higherThanCell && righterThanCell
      = Just (BoardWall
               ( ((row, col),     (row, col + 1))
               , ((row + 1, col), (row + 1, col + 1))))
  | otherwise = Nothing -- Why we need this?
  where
    (minX, minY) = cellCenter (minCol, minRow)
    shiftX = round (minX - (fromIntegral cellSize / 2))
    shiftY = round (minY - (fromIntegral cellSize / 2))
    y' = round x - shiftX
    x' = round y - shiftY
    (xDiv, xMod) = divMod x' cellSizeAndMargin
    (yDiv, yMod) = divMod y' cellSizeAndMargin
    row = yDiv
    higherThanCell = yMod > cellSize
    col = xDiv
    righterThanCell = xMod > cellSize

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
    Just (BoardCell a) -> GuiState gameState (highlightCell a c)
    Just (BoardWall w) -> GuiState gameState (wallHighlighting w)
    Nothing -> GuiState gameState blank
  where
    (GuiState gameState _) = gs
    c = colorPlayer (pcolor (currentPlayer gameState))
    wallHighlighting w
      | tryMove (PutWall w) gameState /= gameState = drawWall hintWallColor w
      | otherwise = blank
handleEvents (EventKey (Char 'r') _ _ _) _ = initiateGame 2
handleEvents _ x = x

window :: Display
window = InWindow "Hooridor" (600, 600) (0, 0)

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
drawCell cell c = translate x y ((color c (rectangleSolid side side)))
  where
    (x, y) = cellCenter cell
    side = fromIntegral cellSize

drawDefaultCell :: Cell -> Picture
drawDefaultCell cell = drawCell cell normalCellColor

highlightCell :: Cell -> Color -> Picture
highlightCell cell c = drawCell cell (dark c)

drawWallSegment :: WallPart -> Color -> Picture
drawWallSegment wp col
  | x1 == x2 = translate x y (color col (rectangleSolid side halfMargin))
  | otherwise = translate x y (color col (rectangleSolid halfMargin side))
  where
    (c1, c2) = wp
    (x1, _) = cellCenter c1
    (x2, _) = cellCenter c2
    (x, y) = segmentCoordinates wp
    side = fromIntegral cellSize
    halfMargin = fromIntegral cellMargin / 2

segmentCoordinates :: WallPart -> (Float, Float)
segmentCoordinates (c1, c2) = (x, y)
  where
    (x1, y1) = cellCenter c1
    (x2, y2) = cellCenter c2
    x = (x1 + x2) / 2
    y = (y1 + y2) / 2

drawWall :: Color  -> Wall  -> Picture
drawWall c (ws1, ws2) = (drawWallSegment ws1 c)
  <> (drawWallSegment ws2 c)
  <> translate x y (color black (rectangleSolid margin margin))
  where
    (x1, y1) = segmentCoordinates ws1
    (x2, y2) = segmentCoordinates ws2
    x = (x1 + x2) / 2
    y = (y1 + y2) / 2
    margin = fromIntegral cellMargin

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
