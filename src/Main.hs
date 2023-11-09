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
    let inky = Ghost Inky Scatter (13, 14) GoRight 1 0.25 0 True
    let clyde = Ghost Clyde Scatter (13, 14) GoRight 1 0.25 0 True
    let pinky = Ghost Pinky Scatter (13, 14) GoRight 1 0.25 0 True
    
    let ghosts = [blinky, inky, pinky, clyde]

    let initialstate = Model.initialState initialPacMan images maze ghosts

    

    playIO (InWindow "PacMan" (448, 580) (0, 0))
             black
             24
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

    scorebmp <- loadBMP "src/sprites/Score.bmp"
    nmr1bmp <- loadBMP "src/sprites/1.bmp"
    nmr2bmp <- loadBMP "src/sprites/2.bmp"
    nmr3bmp <- loadBMP "src/sprites/3.bmp"
    nmr4bmp <- loadBMP "src/sprites/4.bmp"
    nmr5bmp <- loadBMP "src/sprites/5.bmp"
    nmr6bmp <- loadBMP "src/sprites/6.bmp"
    nmr7bmp <- loadBMP "src/sprites/7.bmp"
    nmr8bmp <- loadBMP "src/sprites/8.bmp"
    nmr9bmp <- loadBMP "src/sprites/9.bmp"
    nmr0bmp <- loadBMP "src/sprites/0.bmp"
    eatingpac2 <- loadBMP "src/sprites/EatingPac2.bmp"
    eatingpac3 <- loadBMP "src/sprites/EatingPac3.bmp"
    pausedbmp <- loadBMP "src/sprites/Paused.bmp"
    gameOver <- loadBMP "src/sprites/GameOver.bmp"
    ready <- loadBMP "src/sprites/Ready.bmp"


    dying0bmp <- loadBMP "src/sprites/dying0.bmp"
    dying1bmp <- loadBMP "src/sprites/dying1.bmp"
    dying2bmp <- loadBMP "src/sprites/dying2.bmp"
    dying3bmp <- loadBMP "src/sprites/dying3.bmp"
    dying4bmp <- loadBMP "src/sprites/dying4.bmp"
    dying5bmp <- loadBMP "src/sprites/dying5.bmp"
    dying6bmp <- loadBMP "src/sprites/dying6.bmp"
    dying7bmp <- loadBMP "src/sprites/dying7.bmp"
    dying8bmp <- loadBMP "src/sprites/dying8.bmp"
    dying9bmp <- loadBMP "src/sprites/dying9.bmp"
    dying10bmp <- loadBMP "src/sprites/dying10.bmp"
    dying11bmp <- loadBMP "src/sprites/dying11.bmp"

    return [bgbmp, pelletbmp, powerupbmp, pacmanbmp, blinkybmp, inkybmp, clydebmp, 
            pinkybmp, scorebmp, nmr0bmp, nmr1bmp, nmr2bmp, nmr3bmp, nmr4bmp, nmr5bmp, 
            nmr6bmp, nmr7bmp, nmr8bmp, nmr9bmp, frightghost, pausedbmp, gameOver, ready, 
            eatingpac2, eatingpac3, dying0bmp,dying1bmp,dying2bmp,dying11bmp,dying3bmp,dying4bmp,dying5bmp,dying6bmp,dying7bmp,dying8bmp,dying9bmp,dying10bmp]