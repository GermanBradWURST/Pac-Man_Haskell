module LoadLevel where

type Tile = Empty | Pellet | Wall | SuperPellet | Barrier

type Maze = [[Tile]]

makeMaze :: Maze
makeMaze = map makeLine (lines file)
        where file <- readFile "src/Level1.txt"

makeLine :: String -> [Tile]
makeLine [] = []
makeLine (x:xs) = makeTile x : makeLine xs

makeTile :: Char -> Tile
makeTile c | c == '#' = Wall
           | c == '-' = Empty
           | c == '+' = Pellet
           | c == '%' = SuperPellet
           | c == '_' = Barrier
