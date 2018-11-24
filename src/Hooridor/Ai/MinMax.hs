module Hooridor.Ai.MinMax where
import Hooridor.Core
import Hooridor.Ai.Trees

-- | Given some scoretree find turn which leads to minimum value
minMaxMinimize :: ScoreTree Score -> (Score, Maybe Turn)
minMaxMinimize (Leaf v) = (v,Nothing) -- value found
minMaxMinimize (Node []) = (maxInt,Nothing) -- Should never happen, but just in case
minMaxMinimize (Node ((turn,subtree):rest))
                        | score<score2 = (score,Just turn) 
                        | otherwise = (score2,otherTurn)
                        where 
                            (score2,otherTurn) = minMaxMinimize (Node rest)
                            (score,_) = minMaxMaximize subtree

maxInt = maxBound :: Int

-- | Given some ScoreTree find turn which leads to maximum value
minMaxMaximize :: ScoreTree Score -> (Score, Maybe Turn)
minMaxMaximize (Leaf v) = (v,Nothing)
minMaxMaximize (Node []) = (0,Nothing) -- Should never happen, but just in case
minMaxMaximize (Node ((turn,subtree):rest)) 
                            | score>score2 = (score,Just turn)
                            | otherwise = (score2,otherTurn)
                            where
                                (score2,otherTurn) = minMaxMaximize (Node rest)
                                (score,_) = minMaxMinimize subtree
