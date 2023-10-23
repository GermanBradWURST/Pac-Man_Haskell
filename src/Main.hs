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
    scorebmp <- loadBMP "src/sprites/SCORE.bmp"
    nmr1bmp <- loadBMP "src/sprites/nmr1.bmp"
    nmr2bmp <- loadBMP "src/sprites/nmr2.bmp"
    nmr3bmp <- loadBMP "src/sprites/nmr3.bmp"
    nmr4bmp <- loadBMP "src/sprites/nmr4.bmp"
    nmr5bmp <- loadBMP "src/sprites/nmr5.bmp"
    nmr6bmp <- loadBMP "src/sprites/nmr6.bmp"
    nmr7bmp <- loadBMP "src/sprites/nmr7.bmp"
    nmr8bmp <- loadBMP "src/sprites/nmr8.bmp"
    nmr9bmp <- loadBMP "src/sprites/nmr9.bmp"
    nmr0bmp <- loadBMP "src/sprites/nmr0.bmp"



    let picturelist = [bgbmp, pelletbmp, powerupbmp, pacmanbmp, g1rbmp, scorebmp,nmr0bmp, nmr1bmp, nmr2bmp, nmr3bmp, nmr4bmp ,nmr5bmp, nmr6bmp, nmr7bmp, nmr8bmp, nmr9bmp]
    let blinky = Ghost Blinky Chase (13, 11) GoRight
    let inky = Ghost Inky Chase (13, 11) GoRight-- 13, 14
    let clyde = Ghost Clyde Scatter (13, 11) GoRight-- 14, 14
    let pinky = Ghost Pinky Scatter (13, 11) GoRight-- 13, 13
    

    let ghosts = [blinky, inky, pinky, clyde]

    let initialstate = Model.initialState initialPacMan picturelist maze ghosts

    playIO (InWindow "PacMan" (448, 580) (0, 0))
             black
             7
             initialstate
             view
             input
             step




