module Main where

import Controller
import Model
import View
import Ghost
import LoadLevel
import LoadImages
import Player

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )
import Graphics.Gloss

main :: IO ()
main = do

    file <- readFile "src/Level1.txt"

    

    let _lines = lines file
    let maze = makeMaze _lines

    bgbmp <- loadBMP "src/images/back_g_e.bmp"
    pelletbmp <- loadBMP "src/images/dot.bmp"
    pacmanbmp <- loadBMP "src/images/pacman1.bmp"
    let listp = [bgbmp, pelletbmp, pacmanbmp]
    let pm = P (13, 23) Player.GoLeft

    playIO (InWindow "Counter" (448, 496) (0, 0)) -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              (initialState pm listp maze)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function