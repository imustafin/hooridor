module Hooridor.Ai.Trees where
import Hooridor.Core

type Score = Int

data StateTree v = StateNode v [(Turn, StateTree v)]
  deriving (Show) 

instance Foldable StateTree where
  foldMap f (StateNode v xs) =
    mappend (f v) (mconcat $ map (foldMap f . snd) xs)

data ScoreTree v = Node [(Turn, ScoreTree v)] |
                    Leaf v
  deriving (Show)

instance Foldable ScoreTree where
  foldMap f (Leaf v) = f v
  foldMap f (Node xs) = mconcat $ map (foldMap f . snd) xs
  

instance Functor StateTree where
    fmap f (StateNode v xs) = StateNode (f v) ((fmap . fmap . fmap $ f) xs)
                      
toLeafValueTree :: StateTree v -> ScoreTree v
toLeafValueTree (StateNode v []) = Leaf v
toLeafValueTree (StateNode _ xs) = Node $ (fmap . fmap $ toLeafValueTree) xs
                    
