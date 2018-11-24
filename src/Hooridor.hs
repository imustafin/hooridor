module Hooridor where
import Hooridor.Gui
import Hooridor.Core
import Hooridor.Ai

run :: IO ()
run = runGui

testAI :: IO()
testAI = putStrLn (show (aiPlayer 2 (initialStateAi 2)))
