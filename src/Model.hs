-- | This module contains the data types
--   which represent the state of the game
module Model where
import LoadLevel
import Graphics.Gloss
import Player

data ViewState = Running | Paused

data GameState = GameState {
                   viewState  :: ViewState
                 , elapsedTime :: Float
                 , player :: Pacman
                 , maze :: Maze
                 , loadpics :: [Picture]
                 }

initialState :: Pacman -> [Picture] -> Maze -> GameState
initialState pm p m = GameState Running 0 pm m p