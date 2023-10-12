-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case viewState gstate of
  Running   -> do
          pictures (loadpics gstate)
  _ -> color green (text "test")