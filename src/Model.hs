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
    , score :: Int
    , ghostTimer :: Int
}

initialState :: PacMan -> [Picture] -> Maze -> [Ghost] -> GameState
initialState pacm p m g = GameState { viewState = Running, elapsedTime = 0 , pacman = initialPacMan, ghosts = g, maze = m , images = p, score = 0,ghostTimer = 0 } 

initialPacMan :: PacMan
initialPacMan = PacMan { PacMan.point = (14,23), PacMan.direction = Loadlevel.GoUp }