module Main where

import Controller
import Model
import View
import Loadlevel
import Ghosts
import PacMan

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO)

import Graphics.Gloss

main :: IO ()
main = do

    levelfile <- readFile "src/Level1.txt"
    let level = lines levelfile
    let maze = makeMaze level

    bgbmp <- loadBMP "src/sprites/back_g_e.bmp"
    pelletbmp <- loadBMP "src/sprites/dot.bmp"
    pacmanbmp <- loadBMP "src/sprites/pacman1.bmp"
    powerupbmp <- loadBMP "src/sprites/powerup1.bmp"
    g1rbmp <- loadBMP "src/sprites/ghost_1_right_1.bmp"

    let picturelist = [bgbmp, pelletbmp, powerupbmp, pacmanbmp, g1rbmp]
    
    let pacman = PacMan (13, 23) GoLeft
    let blinky = Ghost Blinky Chase (13, 14) GoRight
    {-
    let inky = G Inky (13, 14) GoRight
    let clyde = G Clyde (13, 14) GoRight
    let pinky = G Pinky (13, 14) GoRight
    -}

    let ghosts = [blinky {-, inky, pinky, clyde-}]

    playIO (InWindow "PacMan" (448, 496) (0, 0))
             white
             10
             (initialState pacman picturelist maze)
             view
             input
             step




