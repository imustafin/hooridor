module Hooridor.Gui.Types where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Renderer w = w -> Picture
type Handler w = Event -> w -> w
type Updater w = Float -> w -> w

type PlayType w
  = Display -> Color -> Int -> w
  -> Renderer w -> Handler w -> Updater w -> IO ()
