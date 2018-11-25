module Hooridor.Gui.Types where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Renderer w = w -> Picture
type Handler w = Event -> w -> w
type Updater w = Float -> w -> w

type RendererIO w = w -> IO Picture
type HandlerIO w = Event -> w -> IO w
type UpdaterIO w = Float -> w -> IO w

data Game w = Game w (Renderer w) (Handler w) (Updater w)
data GameIO w = GameIO w (RendererIO w) (HandlerIO w) (UpdaterIO w)

type PlayType w
  = Display -> Color -> Int -> w
  -> Renderer w -> Handler w -> Updater w -> IO ()

type PlayTypeIO w
  = Display -> Color -> Int -> w
  -> RendererIO w -> HandlerIO w -> UpdaterIO w -> IO ()
