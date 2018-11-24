module Hooridor.Ai.AlphaBeta(alphaBetaMaximize,alphaBetaMinimize) where
import Hooridor.Core
import Hooridor.Ai.Trees

alphaBetaMaximize :: ScoreTree Score -> (Score,Maybe Turn)
alphaBetaMaximize = maximize minInt maxInt Nothing 
maxInt = maxBound :: Int
minInt = minBound :: Int
alphaBetaMinimize :: ScoreTree Score -> (Score,Maybe Turn)
alphaBetaMinimize = minimize minInt maxInt Nothing

-- | Given some scoretree find turn which leads to minimum value
minimize :: Score -- alpha 
            -> Score -- beta
            -> Maybe (Score, Turn) 
            -> ScoreTree Score
            -> (Score, Maybe Turn)
minimize _ _ _ (Leaf v) = (v,Nothing) -- value found
minimize _ _ Nothing (Node []) = undefined
minimize _ _ (Just (score,move)) (Node []) = (score,Just move)
minimize alpha beta best (Node ((turn,subTree):rest))
                        | score<=alpha = (score,Just turn) 
                        | otherwise = minimize alpha newBeta newBest (Node rest)
                        where 
                            (score,_) = maximize alpha beta Nothing subTree
                            newBest = case best of
                                Nothing -> Just (score,turn)
                                Just (accScore, _) -> if score < accScore 
                                    then Just (score,turn)
                                    else best
                            newBeta = min beta score

-- | Given some ScoreTree find turn which leads to maximum value
maximize :: Score 
            -> Score 
            -> Maybe (Score, Turn)
            -> ScoreTree Score 
            -> (Score, Maybe Turn)
maximize _ _ _ (Leaf v) = (v,Nothing)
maximize _ _ Nothing (Node []) = undefined 
maximize _ _ (Just (score,move)) (Node []) = (score,Just move)
maximize alpha beta best (Node ((turn,subTree):rest)) 
                            | score>=beta = (score,Just turn)
                            | otherwise = maximize newAlpha beta newBest (Node rest)
                            where
                                (score,_) = minimize alpha beta Nothing subTree
                                newBest = case best of
                                    Nothing -> Just (score,turn)
                                    Just (accScore, _) -> if score > accScore 
                                        then Just (score,turn)
                                        else best
                                newAlpha = max alpha score
    