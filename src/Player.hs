module Player where

import LoadLevel


data Direction = GoUp | GoDown | GoLeft | GoRight deriving(Eq, Ord)

data Pacman = P LoadLevel.Point Direction


--handling direction changes, pacman can't move in a certain direction if that would lead him 
--directly into a wall
changeDirection :: Pacman -> Direction -> Maze -> Pacman
changeDirection (P (x,y) pacdirec) direc maze 
    |(validMove (P(x,y) pacdirec) direc maze) = (P (x,y) direc)
    |otherwise = (P (x,y) pacdirec)

--helper function to check validity of a move
validMove :: Pacman -> Direction -> Maze -> Bool
validMove (P (x,y) pacdirec) direc maze = (snd nextTile) /= Wall || (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |direc == GoUp = rowUp !! x
                    |direc == GoDown = rowDown !! x
                    |direc == GoRight = currentRow !! (x + 1)
                    |direc == GoLeft = currentRow !! (x - 1)                        

--allowing pacman to move, which as of now is just teleporting 1 block
movePacman :: Pacman -> Maze -> Pacman
movePacman (P (x,y) pacdirec) maze 
    |pacdirec == GoUp = (P (x,y+1) pacdirec)
    |pacdirec == GoDown = (P (x, y-1) pacdirec)
    |pacdirec == GoRight = (P (x + 1,y) pacdirec)
    |pacdirec == GoLeft = (P (x - 1, y) pacdirec)


canMove :: Pacman -> Maze -> Bool
canMove (P (x,y) pacdirec) maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |pacdirec == GoUp = rowUp !! x
                    |pacdirec == GoDown = rowDown !! x
                    |pacdirec == GoRight = currentRow !! (x + 1)
                    |pacdirec == GoLeft = currentRow !! (x - 1) 

keyToDirection :: Char -> Direction
keyToDirection c    | c == 'w' = GoUp
                    | c == 's' = GoDown
                    | c == 'a' = GoLeft
                    | c == 'd' = GoRight


