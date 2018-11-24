module Hooridor where
import Hooridor.Gui
import Hooridor.Core
import Hooridor.Ai

run :: IO ()
run = testAI

play :: IO()
play = playGame 8 2

testAI :: IO()
testAI = putStrLn (show (aiPlayer (initialState 2)))
