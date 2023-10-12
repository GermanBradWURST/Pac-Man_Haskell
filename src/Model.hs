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
                 , player :: PacMan
                 , maze :: Maze
                 , loadpics :: [Picture]
                 }

initialState :: PacMan -> [Picture] -> Maze -> GameState
initialState pm p m = GameState Running 0 pm m p