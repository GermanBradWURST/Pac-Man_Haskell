module Controller where

import Model
import Ghosts
import PacMan
import Loadlevel

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Concurrent (dupChan)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  
  = do
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate 
inputKey _ gstate = gstate -- Otherwise keep the same

--handling direction changes, pacman can't move in a certain direction if that would lead him 
--directly into a wall
changeDirection :: PacMan -> Direction -> Maze -> PacMan
changeDirection (PacMan (x,y) pacdirec) direc maze 
    |(validMove (PacMan(x,y) pacdirec) direc maze) = (PacMan (x,y) direc)
    |otherwise = (PacMan (x,y) pacdirec)

--helper function to check validity of a move
validMove :: PacMan -> Direction -> Maze -> Bool
validMove (PacMan (x,y) pacdirec) direc maze = (snd nextTile) /= Wall || (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |direc == GoUp = rowUp !! x
                    |direc == GoDown = rowDown !! x
                    |direc == GoRight = currentRow !! (x + 1)
                    |direc == GoLeft = currentRow !! (x - 1)                        

--allowing pacman to move, which as of now is just teleporting 1 block
movePacman :: PacMan -> Maze -> PacMan
movePacman (PacMan (x,y) pacdirec) maze 
    |pacdirec == GoUp = (PacMan (x,y+1) pacdirec)
    |pacdirec == GoDown = (PacMan (x, y-1) pacdirec)
    |pacdirec == GoRight = (PacMan (x + 1,y) pacdirec)
    |pacdirec == GoLeft = (PacMan (x - 1, y) pacdirec)


canMove :: PacMan -> Maze -> Bool
canMove (PacMan (x,y) pacdirec) maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |pacdirec == GoUp = rowUp !! x
                    |pacdirec == GoDown = rowDown !! x
                    |pacdirec == GoRight = currentRow !! (x + 1)
                    |pacdirec == GoLeft = currentRow !! (x - 1) 