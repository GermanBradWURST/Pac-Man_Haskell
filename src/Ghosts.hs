module Ghosts where

import Loadlevel

data GhostType = Blinky | Inky | Clyde | Pinky

data Mode = Frightened | Scatter | Chase

data Ghost = Ghost {
      ghosttype :: GhostType
    , mode :: Mode 
    , point :: Loadlevel.Point 
    , direction :: Loadlevel.Direction
}