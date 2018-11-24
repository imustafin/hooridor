module Hooridor.Gui.AiMenuScreen( withAiMenuScreen ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Hooridor.Gui.Types

data AIMenuState a = AIMenu Int (Maybe a)

withAiMenuScreen
  :: (Int -> a) -> PlayType (AIMenuState a)
  -> Display -> Color -> Int -> (Renderer a) -> (Handler a) -> (Updater a)
  -> IO ()
withAiMenuScreen
  makeInitial play' window' background' fps' renderer handler updater
  = play' window' background' fps' initial' render' handle' update'
  where
    initial' = AIMenu 0 Nothing
    render' (AIMenu pc Nothing) = text (show pc)
    render' (AIMenu _ (Just a)) = renderer a
    handle' (EventKey (SpecialKey KeyUp) Down _ _) (AIMenu pc Nothing)
      = AIMenu (pc + 1) Nothing
    handle' (EventKey (SpecialKey KeyEnter) _ _ _) (AIMenu pc Nothing)
      = AIMenu pc (Just (makeInitial pc))
    handle' (EventKey (Char 'r') _ _ _) (AIMenu pc (Just _))
      = AIMenu pc (Just (makeInitial pc))
    handle' e (AIMenu pc (Just a)) = AIMenu pc (Just (handler e a))
    handle' _ (AIMenu pc Nothing) = AIMenu pc Nothing
    update' f (AIMenu pc (Just a)) = AIMenu pc (Just (updater f a))
    update' _ (AIMenu pc Nothing) = AIMenu pc Nothing
