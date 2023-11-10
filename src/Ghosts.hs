module Ghosts where

import Loadlevel

data GhostType = Blinky | Inky | Clyde | Pinky deriving (Show, Eq)

data Mode = Frightened | Scatter | Chase deriving (Show, Eq)

-- inHouse is a bool which tracks wether the ghost is in the ghosthouse or not, this is needed for letting a ghost out of the house.
-- We wouldn't want the ghost to teleport to above the ghosthouse when it has already left the house.

-- gTimer is there to time the switching between modes,
-- and turnTimer is there to prevent the Ghost from getting stuck in a turning loop at an intersection

data Ghost = Ghost {
      ghosttype :: GhostType
    , mode :: Mode 
    , gpoint :: Loadlevel.Point 
    , gdirection :: Loadlevel.Direction
    , gTimer :: Int
    , speed :: Float
    , turnTimer :: Int
    , inHouse :: Bool
}

-- this function is called when a ghost crosses a certain point and teleports them to the other side of the maze

wrapAroundGhost :: Ghost -> Ghost
wrapAroundGhost g | gdirection g == GoRight = g {gpoint = (0, 14)}
                  | gdirection g == GoLeft = g {gpoint = (27, 14)}