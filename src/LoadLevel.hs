module Loadlevel where

data Direction = GoLeft | GoRight | GoUp | GoDown deriving (Show, Eq)

-- We make a maze made up of tiles (which are tuples of their point on the board and the tiletype)
-- we included the point in the tile (as opposed to using index), 
-- because having to translate a point to an index in maze could invite unnecessary trouble later down the line

type Point = (Float, Float)

type Tile = (Point, Tiletype)

data Tiletype = Empty | Pellet | Wall | SuperPellet | Barrier deriving (Show, Eq)

type Maze = [[Tile]]

makeMaze :: [String] -> Maze
makeMaze file = map makeLine (zip [0..] file)

makeLine :: (Float, String) -> [Tile]
makeLine (y, s) = map (mergeInfo y) (map makeTile (zip [0..] s))

makeTile :: (Float, Char) -> (Float, Tiletype)
makeTile (x, c) | c == '#' = (x, Wall)
                | c == '-' = (x, Empty)
                | c == '+' = (x, Pellet)
                | c == '%' = (x, SuperPellet)
                | c == '_' = (x, Barrier)

mergeInfo :: Float -> (Float, Tiletype) -> Tile
mergeInfo y (x, t) = ((x, y), t) 
