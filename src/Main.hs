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

    images <- loadImages

    let blinky = Ghost Blinky Scatter (13, 11) GoRight 1 0.25 0 False
    let inky = Ghost Inky Scatter (13, 14) GoRight 1 0.25 0 True-- 13, 14
    let clyde = Ghost Clyde Scatter (13, 14) GoRight 1 0.25 0 True-- 14, 14
    let pinky = Ghost Pinky Scatter (13, 14) GoRight 1 0.25 0 True-- 13, 13
    
    let ghosts = [blinky, inky, pinky, clyde]

    let initialstate = Model.initialState initialPacMan images maze ghosts

    playIO (InWindow "PacMan" (448, 580) (0, 0))
             black
             23
             initialstate
             view
             input
             step


-- Load BMP images
loadImages :: IO [Picture]
loadImages = do
    bgbmp <- loadBMP "src/sprites/back_g_e.bmp"
    pelletbmp <- loadBMP "src/sprites/dot.bmp"
    pacmanbmp <- loadBMP "src/sprites/pacman1.bmp"
    powerupbmp <- loadBMP "src/sprites/powerup1.bmp"
    blinkybmp <- loadBMP "src/sprites/Blinky.bmp"
    inkybmp <- loadBMP "src/sprites/Inky.bmp"
    clydebmp <- loadBMP "src/sprites/Clyde.bmp"
    pinkybmp <- loadBMP "src/sprites/Pinky.bmp"
    frightghost <- loadBMP "src/sprites/frightGhost.bmp"

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

    pausedbmp <- loadBMP "src/sprites/Paused.bmp"
    gameOver <- loadBMP "src/sprites/GameOver.bmp"

    eatingpac2 <- loadBMP "src/sprites/EatingPac2.bmp"
    eatingpac3 <- loadBMP "src/sprites/EatingPac3.bmp"

    return [bgbmp, pelletbmp, powerupbmp, pacmanbmp, blinkybmp, inkybmp, clydebmp, pinkybmp, 
            scorebmp, nmr0bmp, nmr1bmp, nmr2bmp, nmr3bmp, nmr4bmp, nmr5bmp, nmr6bmp, nmr7bmp, 
            nmr8bmp, nmr9bmp, frightghost, pausedbmp, gameOver, eatingpac2, eatingpac3]