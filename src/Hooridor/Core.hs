module Hooridor.Core where

type Cell = (Int, Int)

data PlayerColor = Green | Yellow | Red | Orange
  deriving (Eq, Enum)

data Player = Player
  { color     :: PlayerColor
  , pos       :: Cell
  , wallsLeft :: Int } deriving (Eq)

data Turn = MakeMove Cell
  | PutWall Wall

type Wall = ((Cell, Cell), (Cell, Cell))
data Board = Board [[Cell]]

data GameState = GameState
  { playerList :: [Player]
  , walls      :: [Wall]
  , winner     :: Maybe PlayerColor }

defaultWalls :: Int
defaultWalls = 5

initialState :: Int -> GameState
initialState playerCount = GameState
  { playerList = take playerCount
    [(initPlayer Green (0,4))
    , (initPlayer Yellow (8,4))
    , (initPlayer Red (4,0))
    , (initPlayer Orange (4,0))]
  , walls = []
  , winner = Nothing }
  where
    initPlayer c p = Player {color = c, pos = p, wallsLeft = defaultWalls}
