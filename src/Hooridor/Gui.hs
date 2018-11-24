module Hooridor.Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Hooridor.Core
import Hooridor.Gui.GameModeMenu
import Hooridor.Ai

-- |State of the GUI: current GameState and additional decoration to draw.
data GuiState = GuiState GameState Picture

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
normalWallColor = magenta

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
    Just (BoardCell c) -> GuiState gameState (cellHighlighting c)
    Just (BoardWall w) -> GuiState gameState (wallHighlighting w)
    Nothing -> GuiState gameState blank
  where
    (GuiState gameState _) = gs
    player = currentPlayer gameState
    wallHighlighting w
      | validTurn (PutWall w) gameState = wallHint w player
      | otherwise = blank
    cellHighlighting c
      | validTurn (MakeMove c) gameState = highlightCell c player
      | otherwise = blank
handleEvents _ x = x

-- |Player icon, not translated.
playerIcon :: Player -> Picture
playerIcon p = color (colorPlayer (pcolor p)) (circleSolid playerRadius)


-- |Player icon translated to the cell position.
drawPlayer :: Player -> Picture
drawPlayer p = translate x y (playerIcon p)
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

-- |Draw a wall hint, translated to the correct position.
wallHint :: Wall -> Player -> Picture
wallHint w p = drawWall ((dark . colorPlayer . pcolor) p) w

-- |Draw all cells normal, translated to the correct positions.
drawBoard :: Picture
drawBoard = pictures cellPictures
  where
    cellPictures
      = map drawDefaultCell allCells
    allCells = [(x, y) | x <- [0..8], y <- [0..8]]

-- |Draw victory screen for this player.
drawVictoryScreen :: Player -> Picture
drawVictoryScreen p
  = translate (-250) 0 (color (dark (colorPlayer (pcolor p))) message)
  <> scale 5 5 (translate 0 (-25) (playerIcon p))
  where
    message = (scale 0.5 1 (text "A winnar is you!"))

-- |Picture telling how many walls this player has, translated on top
-- of the board.
currentPlayerIndicator :: Player -> Picture
currentPlayerIndicator p = translate x y messagePicture <> playerCircle
  where
    (x, y) = cellCenter (maxCol `div` 4, maxRow + 1)
    messageText = "Walls Left: " ++ (show (wallsLeft p))
    messagePicture = scale 0.2 0.2 (text messageText)
    playerCircle
      = translate xShift yShift
          (drawPlayer (p {pos = (maxCol `div` 4 - 1, maxRow + 1)}))
    xShift = fromIntegral cellSize / 2
    yShift = fromIntegral cellSize / 4

-- |Draw current game state
render :: GuiState -> Picture
render (GuiState gameState decorations) =
  case winner gameState of
    Nothing -> drawBoard
            <> decorations
            <> currentPlayerIndicator (currentPlayer gameState)
            <> pictures (map drawPlayer (players))
            <> pictures (map (drawWall normalWallColor) (walls gameState))
    Just winnerPlayer -> drawVictoryScreen winnerPlayer
    where
      players = playerList (gameState)

-- |Create new GuiState with this number of players.
initiateGame :: GMMenuResult -> GuiState
initiateGame (LocalGame pc) = GuiState (initialState pc) blank
initiateGame EasyAi = GuiState (initialStateAi 1) blank
initiateGame HardAi = GuiState (initialStateAi 2) blank

-- |Update per time (nothing updates).
update :: Float -> GuiState -> GuiState
update _ state = case inteligence player of
      (AI depth) -> GuiState (aiPlayer depth gameState) board
      Human -> state
  where
    (GuiState gameState board) = state
    player = currentPlayer gameState

-- |Run the game starting with the menus.
runGui :: IO ()
runGui = (withGameModeMenu initiateGame play)
        window background fps render handleEvents update
