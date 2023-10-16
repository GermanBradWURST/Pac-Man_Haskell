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

initialState :: PacMan -> [Ghost] -> [Picture] -> Maze -> GameState
initialState pman gs ps m = GameState Running 0 pman gs m ps 