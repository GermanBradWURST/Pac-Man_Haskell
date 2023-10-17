module View where

import Model
import PacMan
import Loadlevel
import Ghosts

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case viewState gstate of
    Running -> do
        let scaledlist = map (Scale 2.0 2.0) (images gstate)
            pacpic = translatePacMan (scaledlist!!3) (pacman gstate)
            blinky = translateGhost (scaledlist!!4) ((ghosts gstate)!!0)
        pictures [scaledlist!!0, pacpic, blinky]



translatePacMan :: Picture -> PacMan -> Picture
translatePacMan pict (PacMan (xs, ys) direc) = translate (x*16-216) (240-(y*16)) d
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


translateGhost :: Picture -> Ghost -> Picture
translateGhost pict (Ghost gtype mode (xs, ys) direc) = translate (x*16-216) (240-(y*16)) d
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