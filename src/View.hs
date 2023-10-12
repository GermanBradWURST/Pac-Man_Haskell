-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case viewState gstate of
  Running   -> do
          let scaledmap = Scale 2.0 2.0 $ head (loadpics gstate)
          let pman = ptrans (Scale 2.0 2.0 $ (loadpics gstate) !! 2) (player gstate)
          pictures [scaledmap, pman]
  _ -> color green (text "test")

ptrans :: Picture -> Pacman -> Picture
ptrans pict (P (xs, ys) direc) = translate ((448/26)*x-224) ((496/31)*y-248) d
        where
                pos = (xs,ys)
                x = (fromIntegral . fst) pos
                y = (fromIntegral . snd) pos
                dp = direc
                d | dp == GoUp = Rotate (-90) pict
                  | dp == GoRight = pict
                  | dp == GoLeft = Rotate 180 pict
                  | dp == GoDown = Rotate 90 pict
                  | otherwise = pict