module Loadlevel where

data Direction = GoLeft | GoRight | GoUp | GoDown deriving (Show, Eq)

type Point = (Int, Int)
type Tile = (Point, Tiletype)
data Tiletype = Empty | Pellet | Wall | SuperPellet | Barrier deriving (Show, Eq)

type Maze = [[Tile]]

makeMaze :: [String] -> Maze
makeMaze file = map makeLine (zip [0..] file)

makeLine :: (Int, String) -> [Tile]
makeLine (y, s) = map (mergeInfo y) (map makeTile (zip [0..] s))

makeTile :: (Int, Char) -> (Int, Tiletype)
makeTile (x, c) | c == '#' = (x, Wall)
                | c == '-' = (x, Empty)
                | c == '+' = (x, Pellet)
                | c == '%' = (x, SuperPellet)
                | c == '_' = (x, Barrier)

mergeInfo :: Int -> (Int, Tiletype) -> Tile
mergeInfo y (x, t) = ((x, y), t) 
