module PacMan where

import Loadlevel

data PacMan = PacMan {
      point :: Loadlevel.Point 
    , direction :: Loadlevel.Direction
}