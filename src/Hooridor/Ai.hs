module Hooridor.Ai where
import Data.Maybe
import Hooridor.Core
import Data.List
import Debug.Trace
import Hooridor.Ai.Helper
import Hooridor.Ai.Trees
import Hooridor.Ai.MinMax
import Hooridor.Ai.AlphaBeta
import Control.Parallel.Strategies

-- |AI algorithm move with this depth of analysis.
aiPlayer :: Int -> GameState -> GameState
aiPlayer depth state = takeTurn a state
  where
    a = fromMaybe (MakeMove (0, 0)) x
    (_, x) = alphaBetaMinimize (trace (show (length prunedTree)) prunedTree)
    prunedTree = toLeafValueTree (fmap score (stateTree))
    stateTree = (prune depth (generateStateTree state))

-- |Generate gameState tree from starting state.
generateStateTree :: GameState -> StateTree GameState
generateStateTree state
  | isWinner (currentPlayer state) = StateNode state [] -- Game is finished
  | otherwise = StateNode state
      (map (\a -> (a, generateStateTree (takeTurn a state)))
      (allTurns state)) --Recursively generate all possible paths

-- |Prune tree, because we don't want to evaluate all states.
prune :: Int -> StateTree s -> StateTree s
prune 0 (StateNode a _) = StateNode a []
prune d (StateNode a xs) = StateNode a $ map (fmap $ prune (d - 1)) xs

-- |Score gamestate for current player.
score :: GameState -> Score
score state = withStrategy strat (length path) - (length o_path)
  where
    path = fromMaybe [state] (getShortestPath state)
    o_path = fromMaybe [state]
      (getShortestPath state {playerList = others ++ [current]})
    (current:others) = playerList state
    strat v = do rpar path; rpar o_path; return v

scoreWithTrace :: GameState -> Score
scoreWithTrace state = length (traceShowId path)
  where
    path = fromMaybe [state] (getShortestPath state)
