module Hooridor.Ai.AStar where

import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashPSQ as PSQ
import Data.HashPSQ (HashPSQ, minView)
import Data.List (foldl')

data AStar a c = AStar { visited  :: !(HashSet a),
                            waiting  :: !(HashPSQ a c ()),
                            score    :: !(HashMap a c),
                            memoHeur :: !(HashMap a c),
                            cameFrom :: !(HashMap a a),
                            end      :: !(Maybe a) }
    deriving (Show)

aStarInit start = AStar { visited  = Set.empty,
                            waiting  = PSQ.singleton start 0 (),
                            score    = Map.singleton start 0,
                            memoHeur = Map.empty,
                            cameFrom = Map.empty,
                            end      = Nothing }

runAStar :: (Hashable a, Ord a, Ord c, Num c)
            => (a -> HashSet a)     -- adjacencies in graph
            -> (a -> a -> c) -- distance function
            -> (a -> c)      -- heuristic distance to goal
            -> (a -> Bool)   -- goal
            -> a             -- starting vertex
            -> AStar a c     -- final state

runAStar graph dist heur goal start = aStar' (aStarInit start)
            where aStar' s
                    = case minView (waiting s) of
                        Nothing            -> s
                        Just (x, _,  _, w') ->
                          if goal x
                            then s { end = Just x }
                            else aStar' $ foldl' (expand x)
                                                 (s { waiting = w',
                                                      visited = Set.insert x (visited s)})
                                                 (Set.toList (graph x `Set.difference` visited s))
                  expand x s y
                    = let v = score s ! x + dist x y
                      in case PSQ.lookup y (waiting s) of
                           Nothing -> link x y v
                                        (s { memoHeur
                                               = Map.insert y (heur y) (memoHeur s) })
                           Just _  -> if v < score s ! y
                                        then link x y v s
                                        else s
                  link x y v s
                     = s { cameFrom = Map.insert y x (cameFrom s),
                           score    = Map.insert y v (score s),
                           waiting  = PSQ.insert y (v + memoHeur s ! y) () (waiting s) }
          
-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStar :: (Hashable a, Ord a, Ord c, Num c) =>
            (a -> HashSet a)     -- ^ The graph we are searching through, given as a function from vertices
                            -- to their neighbours.
            -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                            -- never be applied to vertices that are not neighbours, so may be undefined
                            -- on pairs that are not neighbours in the graph.
            -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                            -- distance, or else the path found may not be minimal.
            -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
            -> a             -- ^ The vertex to start searching from.
            -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStar graph dist heur goal start
    = let s = runAStar graph dist heur goal start
        in case end s of
            Nothing -> Nothing
            Just e  -> Just (reverse . takeWhile (not . (== start)) . iterate (cameFrom s !) $ e)
