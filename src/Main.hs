module Main where

import Controller
import Model
import View
import Ghost
import LoadLevel

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO )

main :: IO ()
main = do

    file <- readFile "src/Level1.txt"

    let _lines = lines file
    let maze = makeMaze _lines

    playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function