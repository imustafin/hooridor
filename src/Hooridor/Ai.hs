module Hooridor.Ai where
import Data.Maybe
import Hooridor.Core
import Data.List

type Score = Int

aiPlayer :: GameState -> GameState
aiPlayer state = takeTurn a state
        where
            a = fromMaybe (MakeMove (0,0)) x
            (_,x) = minimize prunedTree
            prunedTree = toLeafValueTree (fmap score (prune 1 (generateStateTree state)))

-- | Maximizes result fo
-- minmax :: StateTree -> Turn

data StateTree v = StateNode v [(Turn, StateTree v)]
  deriving (Show) 

data ScoreTree v = Node [(Turn, ScoreTree v)] |
                    Leaf v
  deriving (Show)

instance Functor StateTree where
    fmap f (StateNode v xs) = StateNode (f v) ((fmap . fmap . fmap $ f) xs)
                      
toLeafValueTree :: StateTree v -> ScoreTree v
toLeafValueTree (StateNode v []) = Leaf v
toLeafValueTree (StateNode _ xs) = Node $ (fmap . fmap $ toLeafValueTree) xs
                    
-- data ScoreTree v = Leaf Int |
wallsShow :: StateTree GameState -> [Wall]
wallsShow (StateNode gs ((_,x):other)) = wallsShow x
wallsShow (StateNode gs []) = walls gs

-- | Generate gameState tree from starting state
generateStateTree :: GameState -> StateTree GameState
generateStateTree state
                | isWinner (currentPlayer state) = StateNode state [] -- Game is finished
                | otherwise = StateNode state 
                        (map (\a -> (a,generateStateTree (takeTurn a state)))
                         (allTurns state)) --Recursively generate all possible paths

-- | Given some scoretree find turn which leads to minimum value
minimize :: ScoreTree Score -> (Score, Maybe Turn)
minimize (Leaf v) = (v,Nothing) -- value found
minimize (Node []) = (maxInt,Nothing) -- Should never happen, but just in case
minimize (Node ((turn,subtree):rest))
                        | score<score2 = (score,Just turn) 
                        | otherwise = (score2,otherTurn)
                        where 
                            (score2,otherTurn) = minimize (Node rest)
                            (score,_) = maximize subtree

maxInt = maxBound :: Int

-- | Given some ScoreTree find turn which leads to maximum value
maximize :: ScoreTree Score -> (Score, Maybe Turn)
maximize (Leaf v) = (v,Nothing)
maximize (Node []) = (0,Nothing) -- Should never happen, but just in case
maximize (Node ((turn,subtree):rest)) 
                            | score>score2 = (score,Just turn)
                            | otherwise = (score2,otherTurn)
                            where
                                (score2,otherTurn) = maximize (Node rest)
                                (score,_) = minimize subtree
    

-- | Prune tree, because we don't want to evaluate all states 
prune :: Int -> StateTree s -> StateTree s
prune 0 (StateNode a _) = StateNode a []
prune d (StateNode a xs) = StateNode a $ map (fmap $ prune (d - 1)) xs

-- | Score gamestate for current player
score :: GameState -> Score
score state = length path
            where
                path = fromMaybe [state] (getShortestPath state)

-- | Generate all possible turns for current player
allTurns :: GameState -> [Turn]
allTurns state = (generateWalls state) ++ (validMoves state)

-- | Generate all possible wall placement turns
generateWalls :: GameState -> [Turn]
generateWalls state = possibleTurns
  where
    possibleTurns = filter (\a -> validTurn a state) allTurns
    allTurns = map PutWall allWalls
    allWalls = concatMap wallsNear allCoords
    wallsNear :: Cell -> [Wall]
    wallsNear c = concatMap (\a ->
                    map (\b -> (a, b)) (wallSegmentsNear c))
                  (wallSegmentsNear c)
    wallSegmentsNear :: Cell -> [WallPart]
    wallSegmentsNear a
      = concatMap (\s -> map (\t -> (s, t)) (coordsNear a)) (coordsNear a)
    coordsNear (a,b)
      = concatMap (\x -> map (\y -> (x, y)) [b-2..b+2]) [a-2..a+2]
    allCoords = concatMap (\x -> map (\y -> (x, y)) [0..8]) [0..8]
