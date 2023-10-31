module Model where

import PacMan
import Ghosts
import Loadlevel
import Graphics.Gloss


data ViewState = Running | Paused | GameOver | Ready | Dying deriving (Show, Eq)

data GameState = GameState {
      viewState :: ViewState
    , elapsedTime :: Float
    , pacman :: PacMan
    , ghosts :: [Ghost]
    , maze :: Maze
    , images :: [Picture]
    , score :: Int
    , ghostTimer :: Float
    , lastPressed :: Char
    , lives :: Int
    , deadCounter :: Int
}

initialState :: PacMan -> [Picture] -> Maze -> [Ghost] -> GameState
initialState pacm p m g = GameState { viewState = Ready, elapsedTime = 0 , pacman = initialPacMan, ghosts = g, maze = m , images = p, score = 0,ghostTimer = 0, lastPressed = 'n', lives = 3 , deadCounter = 0} 

initialPacMan :: PacMan
initialPacMan = PacMan { PacMan.point = (14,23), PacMan.direction = Loadlevel.GoUp }