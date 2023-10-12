module LoadLevel where

type Tile = Empty | Pellet | Wall | SuperPellet | Barrier

type Maze = [[Tile]]

MakeMaze :: Maze
MakeMaze = map MakeLine (lines file)
        where file <- readFile "src/Level1.txt"

MakeLine :: String -> [Tile]
MakeLine [] = []
MakeLine (x:xs) = MakeTile x : MakeLine xs

MakeTile :: Char -> Tile
MakeTile c | c == '#' = Wall
           | c == '-' = Empty
           | c == '+' = Pellet
           | c == '%' = SuperPellet
           | c == '_' = Barrier
