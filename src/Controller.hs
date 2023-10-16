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
input e gstate = return (inputKey e (pacman gstate) gstate )


{-
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)
-}

inputKey :: Event -> PacMan -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) (PacMan (x,y) d) gstate  -- If the user presses a character key, show that one
    |c == 's' = gstate {pacman = (movePacMan (PacMan (x,y) Loadlevel.GoDown) gstate)}
    |c == 'w' = gstate {pacman = (movePacMan (PacMan (x,y) Loadlevel.GoUp) gstate)} 
    |c == 'a' = gstate {pacman = (movePacMan (PacMan (x,y) Loadlevel.GoLeft) gstate)}
    |c == 'd' = gstate {pacman = (movePacMan (PacMan (x,y) Loadlevel.GoRight) gstate)}
inputKey _ _ gstate = gstate -- Otherwise keep the same


inputToDirection :: Event -> Direction
inputToDirection (EventKey (Char c) _ _ _) 
    |c == 'w' = Loadlevel.GoUp
    |c == 's' = Loadlevel.GoDown
    |c == 'a' = Loadlevel.GoLeft
    |c == 'd' = Loadlevel.GoRight


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
movePacMan :: PacMan -> GameState -> PacMan
movePacMan p@(PacMan (x,y) pacdirec) gstate 
    |(pacdirec == GoUp) && canMove p (maze gstate) = (PacMan (x,y-1) pacdirec)
    |pacdirec == GoDown && canMove p (maze gstate) = (PacMan (x, y+1) pacdirec)
    |pacdirec == GoRight && canMove p (maze gstate) = (PacMan (x + 1,y) pacdirec)
    |pacdirec == GoLeft && canMove p (maze gstate) = (PacMan (x - 1, y) pacdirec)
    |otherwise = p
            

-- checking if pacman is able to move or if this moves him into a wall
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