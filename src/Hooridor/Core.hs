module Hooridor.Core where
import Data.HashSet (HashSet,fromList)
import Data.Hashable
import Data.Graph.AStar

type Cell = (Int, Int)

data PlayerColor = Green | Yellow | Red | Orange
  deriving (Eq, Enum, Show,Ord)

data Player = Player
  { pcolor     :: PlayerColor
  , pos       :: Cell
  , wallsLeft :: Int } deriving (Eq, Show,Ord)

data Turn = MakeMove Cell
  | PutWall Wall deriving (Show)

type WallPart = (Cell, Cell)
type Wall = (WallPart, WallPart)


data GameState = GameState
  { playerList :: [Player]
  , walls      :: [Wall]
  , winner     :: Maybe PlayerColor } deriving (Eq, Show,Ord)

instance Hashable GameState where
  hashWithSalt s _ = s

minRow :: Int
minRow = 0

minCol :: Int
minCol = 0

maxRow :: Int
maxRow = size - 1

maxCol :: Int
maxCol = size - 1

size :: Int
size = 9

minRow :: Int
minRow = 0

minCol :: Int
minCol = 0

maxRow :: Int
maxRow = size - 1

maxCol :: Int
maxCol = size - 1

size :: Int
size = 9

cellInBound :: Cell -> Bool
cellInBound (x, y) = 0 <= x && x <= 8 && 0 <= y && y <= 8

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

areAdjacentCells :: Cell -> Cell -> Bool
areAdjacentCells (x1, y1) (x2, y2)
  = abs (x1 - x2) + abs (y1 - y2) == 1


isValidWallPart :: WallPart -> Bool
isValidWallPart (c1, c2)
  = areAdjacentCells c1 c2 && cellInBound c1 && cellInBound c2

isValidWall :: Wall -> Bool
isValidWall ((c1, c2), (c3, c4))
  = isValidWallPart (c1, c2) && isValidWallPart (c3, c4)
  && ((areAdjacentCells c1 c3 && areAdjacentCells c2 c4)
      || (areAdjacentCells c2 c3 && areAdjacentCells c1 c4))
  && c1 /= c2 && c1 /= c3 && c1 /= c4
  && c2 /= c3 && c2 /= c4 && c3 /= c4

notInWall :: WallPart -> [Wall] -> Bool
notInWall step walls' = not (any (hasWallPart step) walls')

currentPlayer :: GameState -> Player
currentPlayer state = head (playerList state)

oneStep :: Cell -> Cell -> [Wall] -> Bool
oneStep (x1, y1) (x2, y2) walls'
      = abs (x1 - x2) + abs (y1 - y2) == 1
      && notInWall ((x1, y1), (x2, y2)) walls'

-- | Perform one turn and give turn order to next player
takeTurn :: Turn -> GameState -> GameState
takeTurn pwall@(PutWall wall) state
  | validTurn pwall state = newstate
  | otherwise = state
  where
    newstate = state {walls = wall : walls state
                     , playerList = others ++ [current {wallsLeft = wallsLeft current - 1}]}
    (current:others) = playerList state

takeTurn mv@(MakeMove (x, y)) state
  | validTurn mv state = newstate
  | otherwise = state
  where
    newstate = state {playerList = others ++ [current {pos = (x, y)}]}
    (current:others) = playerList state

-- | Checks if Turn is valid
validTurn :: Turn -> GameState -> Bool
validTurn (MakeMove cell) state =
          cellInBound (x, y) &&
          emptyCell &&
          (canShortCutTo
            || oneStep (pos current) (x, y) (walls state))
        where
          (current:others) = playerList state
          (x, y) = cell
          emptyCell = all (\p -> (x, y) /= pos p) (playerList state)
          canShortCutTo =
                    any
                    (\o -> oneStep (pos current) (pos o) (walls state)
                    && oneStep (pos o) (x, y) (walls state)) others

validTurn (PutWall wall) state =
          hasWalls &&
          isInBounds &&
          not intersect &&
          isValidWall wall &&
          playersCanReachGoal (state {walls = wall : walls state} )
        where
          current = currentPlayer state
          ((cell1,cell2), (cell3, cell4)) = wall
          isInBounds = all cellInBound [cell1, cell2, cell3, cell4]
          wall' = transposeWall wall
          coincide (part1, part2)
            = hasWallPart part1 wall || hasWallPart part2 wall
          hasWalls = wallsLeft current > 0
          intersect = any (\w -> coincide w || wallEq w wall') (walls state)

-- | Generate new state from movement, but don't pass turn
move ::  GameState -> Turn -> GameState
move state mv 
  | validTurn mv state = newstate
  | otherwise = state
  where
    MakeMove (x, y) = mv
    newstate = state {playerList = (current {pos = (x, y)}):others}
    (current:others) = playerList state

-- | Get all possible moves 
validMoves :: GameState -> [Turn]
validMoves state = filter ((/= state) . (`takeTurn` state)) allMoves
  where
    (currentX, currentY) = pos (currentPlayer state)
    minX = currentX - 2
    minY = currentY - 2
    maxX = currentX + 2
    maxY = currentY + 2
    allMoves = concatMap (\x-> map (\y-> MakeMove (x,y)) [minY..maxY]) [minX..maxX]

isWinner :: Player -> Bool
isWinner player = winAt (pcolor player) (pos player)
  where
    winAt Green (8, _) = True
    winAt Yellow (0, _) = True
    winAt Red (_, 8) = True
    winAt Orange (_, 0) = True
    winAt _ _ = False

-- | Check if all players can reach final state
playersCanReachGoal :: GameState -> Bool
playersCanReachGoal state = all (playerCanReachGoal state) (playerList state)

-- | Check if player can reach final state
playerCanReachGoal :: GameState -> Player -> Bool
playerCanReachGoal state player = 
                    case path of
                      Just a -> True
                      Nothing -> False
                    where
                      path = getShortestPath (giveTurn state player)

playerCanReachGoalOld state player = dfs [] (pos player)
  where
    dfs visited p
      | isWinner (player {pos=p}) = True
      | elem p visited = False
      | otherwise = any (\p' -> dfs visited' p')
          (availablePositions state player p)
      where
      visited' = p : visited

-- | Give turn to some player
giveTurn :: GameState -> Player -> GameState
giveTurn state p = putPlayerOn state p (pos p)


-- | In this GameState this Player standing on this Cell
-- | to what cells can go in one step
availablePositions :: GameState -> Player -> Cell -> [Cell]
availablePositions state player p
  = map (pos . last . playerList . (\m -> takeTurn m makeState))
      (validMoves makeState)
      where
        makeState = putPlayerOn state player p

-- | Make a state where it is this player on given cell an it's his/her turn to move
putPlayerOn :: GameState -> Player -> Cell -> GameState
putPlayerOn state player p = state { playerList = makePlayerList }
    where
    makePlayerList = (player {pos=p}) : playerListWithoutThis
    -- | All players without this player
    playerListWithoutThis
      = filter ((/= pcolor player) . pcolor) (playerList state)

-- | Get shortest path to victory of current player
getShortestPath :: GameState -> Maybe [GameState]
getShortestPath start = aStar next cost heuristic won start
        where
         -- next :: GameState -> HashSet GameState
          next a = fromList (map (move a) (validMoves a))
          -- | Constant cost
          cost _ _ = 1 
          heuristic _ = 100 -- | distance to 
          won = isWinner . currentPlayer
          

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
