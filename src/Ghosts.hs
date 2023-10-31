module Ghosts where

import Loadlevel

data GhostType = Blinky | Inky | Clyde | Pinky deriving (Show, Eq)

data Mode = Frightened | Scatter | Chase deriving (Show, Eq)

data Ghost = Ghost {
      ghosttype :: GhostType
    , mode :: Mode 
    , gpoint :: Loadlevel.Point 
    , gdirection :: Loadlevel.Direction
    , gTimer :: Int
    , speed :: Float
    , turnTimer :: Int
}

wrapAroundGhost :: Ghost -> Ghost
wrapAroundGhost g | gdirection g == GoRight = g {gpoint = (0, 14)}
                  | gdirection g == GoLeft = g {gpoint = (27, 14)}