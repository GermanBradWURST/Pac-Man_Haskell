module Model where

import PacMan
import Ghosts
import Loadlevel
import Graphics.Gloss


data ViewState = Running | Paused | GameOver deriving (Show, Eq)

data GameState = GameState {
      viewState :: ViewState
    , elapsedTime :: Float
    , pacman :: PacMan
    , ghosts :: [Ghost]
    , maze :: Maze
    , images :: [Picture]
}

initialState :: PacMan -> [Picture] -> Maze -> GameState
initialState pm p m = GameState { viewState = Running, elapsedTime = 0 , pacman = initialPacman, ghosts = [blinky], maze = m , images = p } 
    where
       blinky = Ghost { ghosttype = Blinky, mode = Scatter, Ghosts.point = (10,10), Ghosts.direction = Loadlevel.GoLeft }

initialPacman :: PacMan
initialPacman = PacMan { PacMan.point = (15,15), PacMan.direction = Loadlevel.GoRight }
