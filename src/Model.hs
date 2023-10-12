-- | This module contains the data types
--   which represent the state of the game
module Model where
import LoadLevel
import Graphics.Gloss

data ViewState = Running | Paused

data GameState = GameState {
                   viewState  :: ViewState
                 , elapsedTime :: Float
                 , maze :: Maze
                 , loadpics :: [Picture]
                 }

initialState :: [Picture] -> Maze -> GameState
initialState p m = GameState Running 0 m p