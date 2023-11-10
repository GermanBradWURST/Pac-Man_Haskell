module PacMan where

import Loadlevel

data PacMan = PacMan {
      point :: Loadlevel.Point 
    , direction :: Loadlevel.Direction
}


-- this function is called when pacman crosses a certain point and teleports them to the other side of the maze

wrapAroundPacMan :: PacMan -> PacMan
wrapAroundPacMan pman | (direction pman) == GoRight = pman {point = (0, 14)}
                      | (direction pman) == GoLeft = pman {point = (27, 14)}