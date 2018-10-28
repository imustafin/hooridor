module Hooridor.Core where

type Cell = (Int, Int)

data PlayerColor = Green | Yellow | Red | Orange
  deriving (Eq, Enum, Show)

data Player = Player
  { color     :: PlayerColor
  , pos       :: Cell
  , wallsLeft :: Int } deriving (Eq, Show)

data Turn = MakeMove Cell
  | PutWall Wall

type Wall = ((Cell, Cell), (Cell, Cell))
data Board = Board [[Cell]]

data GameState = GameState
  { playerList :: [Player]
  , walls      :: [Wall]
  , winner     :: Maybe PlayerColor } deriving (Show)

tryMove :: Turn -> GameState -> GameState
tryMove (MakeMove (x, y)) state
  | isInBounds && emptyCell &&
    (canShortCutTo || oneStep (pos current) (x, y)) = newstate
  | otherwise = state
  where
    newstate = state {playerList = others ++ [current {pos = (x, y)}]}
    (current:others) = playerList state
    isInBounds = 0 < x && x < 9 && 0 < y && y < 9
    emptyCell = all (\p -> (x, y) /= pos p) (playerList state)
    oneStep (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) == 1
    canShortCutTo = any (\o -> oneStep (pos current) (pos o) &&
                               oneStep (pos o) (x, y))
                    others

defaultWalls :: Int
defaultWalls = 5

initialState :: Int -> GameState
initialState playerCount = GameState
  { playerList = take playerCount
    [(initPlayer Green (0,4))
    , (initPlayer Yellow (1,4))
    , (initPlayer Red (4,0))
    , (initPlayer Orange (4,0))]
  , walls = []
  , winner = Nothing }
  where
    initPlayer c p = Player {color = c, pos = p, wallsLeft = defaultWalls}
