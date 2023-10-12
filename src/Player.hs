module Player where

import Controller
import Model
import View
import Main
import LoadLevel

type Point = (Int, Int)
data Direction = Up | Down | Left | Right

data PacMan = P Point Direction


--handling direction changes, pacman can't move in a certain direction if that would lead him 
--directly into a wall
changeDirection :: Pacman -> Direction -> Maze -> Pacman
changeDirection (P (x,y) pacdirec) direc maze =
    | (validMove maze) = (P (x,y) direc)
    | Otherwise = (P (x,y) pacdirec)

--helper function to check validity of a move
validMove :: Pacman -> Direction -> Maze -> Bool
validMove (P (x,y) pacdirec) direc m = (snd nextTile) /= Wall || (snd nextTile) /= Barrier
where
    currentRow = maze !! y
    rowUp = maze !! (y-1)
    rowDown = maze !! (y+1)
    nextTile =  |direc == Up = rowUp !! x
                |direc == Down = rowDown !! x
                |direc == Right = currentRow !! (x + 1)
                |direc == Left = currentRow !! (x - 1)                        

--allowing pacman to move, which as of now is just teleporting 1 block
movePacman :: Pacman -> Maze -> Pacman
movePacman (P (x,y) pacdirec) maze =
    |pacdirec == Up = (P (x,y+1) pacdirec)
    |pacdirec == Down = (P (x, y-1) pacdirec)
    |pacdirec == Right = (P (x + 1,y) pacdirec
    |pacdirec == Left = (P (x - 1, y) pacdirec)

canMove :: Pacman -> Maze -> Bool
canMove (P (x,y) pacdirec) maze = (snd nextTile) /= Wall || (snd nextTile) /= Barrier
where
    currentRow = maze !! y
    rowUp = maze !! (y-1)
    rowDown = maze !! (y+1)
    nextTile =  |pacdirec == Up = rowUp !! x
                |pacdirec == Down = rowDown !! x
                |pacdirec == Right = currentRow !! (x + 1)
                |pacdirec == Left = currentRow !! (x - 1) 