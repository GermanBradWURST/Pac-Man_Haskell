module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

data GameState = GameState { playfield :: PlayField
                , field :: Field
                , player1 :: Player
                , ghost1 :: Ghost
                , ghost2 :: Ghost
                , ghost3 :: Ghost
                , ghost4 :: Ghost
                , normalPellet :: Pellet
                , powerPellet :: Pellet
                }

data Ghost = G1 | G2 | G3 | G4
data Player = P1
data Pellet = NormalPellet | PowerPellet
data Field = Player | Ghost | X | P | E | W
    deriving (Eq, Ord)

type Row = (Field, Field, Field, Field, Field, Field, Field, Field, Field, Field)
type PlayField = (Row, Row, Row, Row, Row, Row, Row, Row, Row, Row)

instance Show Ghost where
    show G1 = "B" -- Blinky
    show G2 = "P" -- Pinky
    show G3 = "I" -- Inky
    show G4 = "C" -- Clyde

instance Show Player where
    show P1 = "C"

instance Show Field where
    show X = "*"
    show P = "."
    show E = " "
    show W = "-"
 


playerMoves :: PlayField -> Player -> [PlayField]
playerMoves p = undefined

printGame :: PlayField -> String
printGame p = undefined

ghostMoves :: PlayField -> Ghost -> PlayField 
ghostMoves = undefined

stratBlinky :: Ghost -> PlayField -> PlayField
stratBlinky = undefined

stratPinky :: Ghost -> PlayField -> PlayField
stratPinky = undefined

stratInky :: Ghost -> PlayField -> PlayField
stratInky = undefined

stratClyde :: Ghost -> PlayField -> PlayField
stratClyde = undefined