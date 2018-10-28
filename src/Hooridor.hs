module Hooridor where

type Cell = (Int, Int)

data PlayerColor = Green | Yellow | Red | Orange
  deriving (Eq, Enum)

data Player = Player
  { color     :: PlayerColor
  , pos       :: Cell
  , gatesLeft :: Int } deriving (Eq)

data Turn

run :: IO ()
run = putStrLn "Hello, world!"
