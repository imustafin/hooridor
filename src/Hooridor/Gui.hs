module Hooridor.Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Hooridor.Core
  (GameState, Cell, Wall, WallPart, PlayerColor (Red, Green, Yellow, Orange)
  , Player, takeTurn, Turn (MakeMove, PutWall), pcolor
  , currentPlayer, pos, walls, isWinner, playerList, initialState, minRow
  , maxRow, minCol, maxCol, size, validTurn)
import Hooridor.Gui.AiMenuScreen

-- |State of the GUI: current GameState and additional decoration to draw.
data GuiState = GuiState GameState Picture
--import Graphics.Gloss.Interface.Pure.Color

type Renderer w = w -> Picture
type Handler w = Event -> w -> w
type Updater w = Float -> w -> w

-- |Board objects that can be pointed at by a mouse.
data BoardObject = BoardCell Cell | BoardWall Wall
  deriving (Show)

-- Game Display Configuration

-- |Default Display to draw the game in.
window :: Display
window = InWindow "Hooridor" (600, 600) (0, 0)

-- |Default window background color.
background :: Color
background = white

-- |Default game frames per second value.
fps :: Int
fps = 60

-- Game Colors

-- |Display color of normal cells
normalCellColor :: Color
normalCellColor = black

-- |Display color of walls
normalWallColor :: Color
normalWallColor = red

-- |Display color of wall hints
hintWallColor :: Color
hintWallColor = (light . light) normalWallColor

-- |Display color of PlayerColors
colorPlayer :: PlayerColor -> Color
colorPlayer Red = red
colorPlayer Green = green
colorPlayer Yellow = yellow
colorPlayer Orange = orange

-- Game Sizes.

-- |Side of square cells in pixels.
cellSize :: Int
cellSize = 40

-- |Radius of the player icon
playerRadius :: Float
playerRadius = fromIntegral cellSize * 0.35

-- |Free space between cells in pixels.
cellMargin :: Int
cellMargin = 10

-- |Distance between sides of two adjacent cells.
--
-- Equal to 'cellSize' plus 'cellMargin':
-- prop> cellSize + cellMargin = cellSizeAndMargin
cellSizeAndMargin :: Int
cellSizeAndMargin = cellSize + cellMargin

-- |Center of the Cell drawn on the screen.
cellCenter :: Cell -> Point
cellCenter (x,y)
  = ( fromIntegral (x - halfBoard) * k
    , fromIntegral (y - halfBoard) * k)
  where
    halfBoard = size `div` 2
    k = fromIntegral cellSizeAndMargin

-- |WallPart center coordinates.
wallPartCenter :: WallPart -> Point
wallPartCenter (c1, c2) = meanPoint (cellCenter c1) (cellCenter c2)

-- |What board object is drawn at this point.
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
  | otherwise = Nothing
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

-- |Response to events.
handleEvents :: Event -> GuiState -> GuiState
handleEvents (EventKey (MouseButton _) Down _ (x', y')) gs =
  case pointingAt (x', y') of
    Just (BoardCell a) -> GuiState (takeTurn (MakeMove a) gameState) board
    Just (BoardWall a) -> GuiState (takeTurn (PutWall a) gameState) board
    Nothing -> gs
  where
    (GuiState gameState board) = gs
handleEvents (EventMotion (x',y')) gs
  = case pointingAt (x', y') of
    Just (BoardCell a) -> GuiState gameState (highlightCell a player)
    Just (BoardWall w) -> GuiState gameState (wallHighlighting w)
    Nothing -> GuiState gameState blank
  where
    (GuiState gameState _) = gs
    player = currentPlayer gameState
    wallHighlighting w
      | validTurn (PutWall w) gameState = drawWall hintWallColor w
      | otherwise = blank
handleEvents _ x = x

-- |Player icon translated to the cell position.
drawPlayer :: Player -> Picture
drawPlayer p
  = translate x y (color (colorPlayer (pcolor p)) (circleSolid playerRadius))
  where
    (x, y) = cellCenter (pos p)

-- |Colored cell translated to the cell position.
drawCell :: Cell -> Color -> Picture
drawCell cell c = translate x y ((color c (rectangleSolid side side)))
  where
    (x, y) = cellCenter cell
    side = fromIntegral cellSize

-- |Cell with the default color translated to the cell position.
drawDefaultCell :: Cell -> Picture
drawDefaultCell cell = drawCell cell normalCellColor

-- |Cell highlighted for this player and translated to the cell position.
highlightCell :: Cell -> Player -> Picture
highlightCell cell p = drawCell cell ((dark . colorPlayer . pcolor) p)

-- |The point between two points.
meanPoint :: Point -> Point -> Point
meanPoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

-- |Draw a wall part with this color, translated to the correct position.
drawWallPart :: WallPart -> Color -> Picture
drawWallPart wp@((x1, _), (x2, _)) col
  = translate x y (color col (rectangleSolid width height))
  where
    (width, height) =
      if x1 == x2
        then (side, halfMargin)
        else (halfMargin, side)
    (x, y) = wallPartCenter wp
    side = fromIntegral cellSize
    halfMargin = fromIntegral cellMargin / 2

-- |Draw wall with this cololr, translated to the correct position.
drawWall :: Color -> Wall -> Picture
drawWall c (wp1, wp2)
  =  (drawWallPart wp1 c)
  <> (drawWallPart wp2 c)
  <> translate x y (color black (rectangleSolid margin margin))
  where
    (x, y) = meanPoint (wallPartCenter wp1) (wallPartCenter wp2)
    margin = fromIntegral cellMargin

-- |Draw all cells normal, translated to the correct positions.
drawBoard :: Picture
drawBoard = pictures cellPictures
  where
    cellPictures
      = concatMap (\x -> map (\y -> drawDefaultCell (x, y)) [0..8]) [0..8]

-- |Draw victory screen for this player.
drawVictoryScreen :: Player -> Picture
drawVictoryScreen p = translate (-220) 0 (color (dark c) message)
  where
    c = colorPlayer (pcolor p)
    message = (scale 0.5 1 (text "You have won!"))

-- |Draw current game state
render :: GuiState -> Picture
render (GuiState gameState decorations) =
  case winner of
    Nothing -> drawBoard <> decorations
            <> pictures (map drawPlayer (players))
            <> pictures (map (drawWall normalWallColor) (walls gameState))
    Just winnerPlayer -> drawVictoryScreen winnerPlayer
    where
      winner = find isWinner players
      players = playerList (gameState)

-- |Create new GuiState with this number of players.
initiateGame :: Int -> GuiState
initiateGame pc = GuiState (initialState pc) blank

-- |Update per time (nothing updates).
update :: Float -> GuiState -> GuiState
update _ state =
        case inteligence player of
            AI -> GuiState (aiPlayer gameState) board
            Human -> state
        where  
            (GuiState gameState board) = state
            player = currentPlayer gameState

-- |Run the game starting with the menus.
runGui :: IO ()
runGui = (withAiMenuScreen (\_ -> initiateGame 2) play)
        window background fps render handleEvents update
