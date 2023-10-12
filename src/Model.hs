-- | This module contains the data types
--   which represent the state of the game
module Model where
import LoadLevel

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , maze :: Maze
                 }

initialState :: Maze -> GameState
initialState = GameState ShowNothing 0