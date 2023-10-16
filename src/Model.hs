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

initialState :: Pacman -> [Picture] -> Maze -> GameState
initialState pm p m = GameState Running 0 initialPacman m p

initialPacman :: Pacman
initialPacman = P (15,15) GoRight