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

initialState :: PacMan -> [Picture] -> Maze -> [Ghost] -> GameState
initialState pacm p m g = GameState { viewState = Running, elapsedTime = 0 , pacman = initialPacman, ghosts = g, maze = m , images = p } 

initialPacman :: PacMan
initialPacman = PacMan { PacMan.point = (15,15), PacMan.direction = Loadlevel.GoRight }
