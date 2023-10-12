module Player where

import Controller
import Model
import View
import Main

type Point = (Int, Int)
data Direction = Up | Down | Left | Right

data PacMan = P Point Direction


--handling movement of PacMan
changeDirection :: Pacman -> Direction -> Pacman
changeDirection (P (x,y) pd) d = (P (x,y) d)
                                