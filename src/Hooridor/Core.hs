module Hooridor.Core where

type Cell = (Int, Int)

data PlayerColor = Green | Yellow | Red | Orange
  deriving (Eq, Enum, Show)

data Player = Player
  { pcolor     :: PlayerColor
  , pos       :: Cell
  , wallsLeft :: Int } deriving (Eq, Show)

data Turn = MakeMove Cell
  | PutWall Wall deriving (Show)

type WallPart = (Cell, Cell)
type Wall = (WallPart, WallPart)


data GameState = GameState
  { playerList :: [Player]
  , walls      :: [Wall]
  , winner     :: Maybe PlayerColor } deriving (Eq, Show)

cellInBound :: Cell -> Bool
cellInBound (x, y) = 0 <= x && x <= 9 && 0 <= y && y <= 9

transposeWall :: Wall -> Wall
transposeWall ((a, b), (c, d)) = ((a, c), (b, d))

wallPartEq :: WallPart -> WallPart -> Bool
wallPartEq (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

wallEq :: Wall -> Wall -> Bool
wallEq (a, b) (c, d)
  = (wallPartEq a c && wallPartEq b d)
  || (wallPartEq a d && wallPartEq b c)

hasWallPart :: WallPart -> Wall -> Bool
hasWallPart a (b, c) = wallPartEq a b || wallPartEq a c

notInWall :: WallPart -> [Wall] -> Bool
notInWall step walls' = not (any (hasWallPart step) walls')

currentPlayer :: GameState -> Player
currentPlayer state = head (playerList state)

oneStep :: Cell -> Cell -> [Wall] -> Bool
oneStep (x1, y1) (x2, y2) walls'
      = abs (x1 - x2) + abs (y1 - y2) == 1
      && notInWall ((x1, y1), (x2, y2)) walls'

tryMove :: Turn -> GameState -> GameState
tryMove (PutWall wall@((cell1,cell2), (cell3, cell4))) state
  | hasWalls && isInBounds && not intersect = newstate
  | otherwise = state
  where
    newstate = state {walls = wall : walls state
                     , playerList = others ++ [current {wallsLeft = wallsLeft current - 1}]}
    (current:others) = playerList state
    isInBounds = all cellInBound [cell1, cell2, cell3, cell4]
    wall' = transposeWall wall
    coincide (part1, part2)
      = hasWallPart part1 wall || hasWallPart part2 wall
    hasWalls = wallsLeft current > 0
    intersect = any (\w -> coincide w || wallEq w wall') (walls state)

tryMove (MakeMove (x, y)) state
  | cellInBound (x, y) && emptyCell &&
    (canShortCutTo || oneStep (pos current) (x, y) (walls state)) = newstate
  | otherwise = state
  where
    newstate = state {playerList = others ++ [current {pos = (x, y)}]}
    (current:others) = playerList state
    emptyCell = all (\p -> (x, y) /= pos p) (playerList state)
    canShortCutTo = any (\o -> oneStep (pos current) (pos o) (walls state) &&
                               oneStep (pos o) (x, y) (walls state))
                    others

validMoves :: GameState -> [Turn]
validMoves state = filter ((/= state) . (`tryMove` state)) allMoves
  where
    (currentX, currentY) = pos (currentPlayer state)
    minX = currentX - 2
    minY = currentY - 2
    maxX = currentX + 2
    maxY = currentY + 2
    allMoves = concatMap (\x-> map (\y-> MakeMove (x,y)) [minY..maxY]) [minX..maxX]

isWinner :: Player -> Bool
isWinner player = winAt ( pcolor player) (pos player)
  where
    winAt Green (8, _) = True
    winAt Yellow (0, _) = True
    winAt Red (_, 8) = True
    winAt Orange (_, 0) = True
    winAt _ _ = False

defaultWalls :: Int
defaultWalls = 5

initialState :: Int -> GameState
initialState playerCount = GameState
  { playerList = take playerCount
    [(initPlayer Green (0,4))
    , (initPlayer Yellow (8,4))
    , (initPlayer Red (4,0))
    , (initPlayer Orange (4,8))]
  , walls = []
  , winner = Nothing }
  where
    initPlayer c p = Player { pcolor = c, pos = p, wallsLeft = defaultWalls}
