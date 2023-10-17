module Controller where

import Model
import Ghosts
import PacMan
import Loadlevel

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Concurrent (dupChan)


searchMaze :: Maze -> (Int, Int) -> Tile
searchMaze m (x, y) = (m!!y)!!x

--Checks for wrap around
checkWrap :: Loadlevel.Point -> Direction -> Bool
checkWrap p d  | p == (0, 14) && d == GoLeft = True
               | p == (27, 14) && d == GoRight = True
               | otherwise = False

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do
    let pman = stepPacMan gstate
    let glist = map (stepGhost gstate) (ghosts gstate)
    let pelletmaze = [[eatPellet tile pman | tile <- row] | row <- maze gstate] -- checking if the current pacman is eating a pellet
    let newmaze = [[eatSuperPellet tile pman | tile <- row] | row <- pelletmaze] -- checking if the current pacman is eating a super pellet
        
    return $ gstate { elapsedTime = elapsedTime gstate + secs, pacman = pman, maze = newmaze, ghosts = glist }

stepPacMan :: GameState -> PacMan
stepPacMan gstate | checkWrap (point (pacman gstate)) (direction (pacman gstate)) = wrapAroundPacMan (pacman gstate)
                  | canMove (pacman gstate) (maze gstate) = movePacMan (pacman gstate) gstate
                  | otherwise = pacman gstate

--wrapAround :: PacMan -> PacMan

-- handles pacman eating pellets (and keeping score)
eatPellet :: Tile -> PacMan -> Tile
eatPellet tile (PacMan (x,y) d) = if ((fst tile) == (x,y)) && (snd tile) == Pellet then ((fst tile), Empty) else tile

-- handles pacman eating super pellets
eatSuperPellet :: Tile -> PacMan -> Tile
eatSuperPellet tile (PacMan (x,y) d) = if ((fst tile) == (x,y)) && (snd tile) == SuperPellet then ((fst tile), Empty) else tile


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e (pacman gstate) gstate)


inputKey :: Event -> PacMan -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) (PacMan (x,y) d) gstate  -- If the user presses a character key, show that one
    |c == 's' = gstate {pacman = changeDirection (pacman gstate) GoDown (maze gstate)}
    |c == 'w' = gstate {pacman = changeDirection (pacman gstate) GoUp (maze gstate)}
    |c == 'a' = gstate {pacman = changeDirection (pacman gstate) GoLeft (maze gstate)}
    |c == 'd' = gstate {pacman = changeDirection (pacman gstate) GoRight (maze gstate)}
inputKey _ _ gstate = gstate -- Otherwise keep the same


--handling direction changes, pacman can't move in a certain direction if that would lead him 
--directly into a wall
changeDirection :: PacMan -> Direction -> Maze -> PacMan
changeDirection (PacMan (x,y) pacdirec) direc maze
    |(validMove (PacMan (x,y) pacdirec) direc maze) = (PacMan (x,y) direc)
    |otherwise = (PacMan (x,y) pacdirec)



--helper function to check validity of a move
validMove :: PacMan -> Direction -> Maze -> Bool
validMove (PacMan (x,y) pacdirec) direc maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |direc == GoUp = rowUp !! x
                    |direc == GoDown = rowDown !! x
                    |direc == GoRight = currentRow !! (x + 1)
                    |direc == GoLeft = currentRow !! (x - 1)

--allowing pacman to move, which as of now is just teleporting 1 block
movePacMan :: PacMan -> GameState -> PacMan
movePacMan p@(PacMan (x,y) pacdirec) gstate
    |(pacdirec == GoUp) && canMove p (maze gstate) = (PacMan (x,y-1) pacdirec)
    |pacdirec == GoDown && canMove p (maze gstate) = (PacMan (x, y+1) pacdirec)
    |pacdirec == GoRight && canMove p (maze gstate) = (PacMan (x + 1,y) pacdirec)
    |pacdirec == GoLeft && canMove p (maze gstate) = (PacMan (x - 1, y) pacdirec)
    |otherwise = p


-- checking if pacman is able to move or if this moves him into a wall
canMove :: PacMan -> Maze -> Bool
canMove (PacMan (x,y) pacdirec) maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        currentRow = maze !! y
        rowUp = maze !! (y-1)
        rowDown = maze !! (y+1)
        nextTile    |pacdirec == GoUp = rowUp !! x
                    |pacdirec == GoDown = rowDown !! x
                    |pacdirec == GoRight = currentRow !! (x + 1)
                    |pacdirec == GoLeft = currentRow !! (x - 1) 

---Ghost step calcs

stepGhost :: GameState -> Ghost -> Ghost
stepGhost gstate g | canChangeDirection gstate g = ghostMoveOne (chooseGhost gstate g)
                   | checkWrap (gpoint g) (gdirection g) = wrapAroundGhost g
                   | otherwise = ghostMoveOne g


--Moves the ghost one forward when it cant turn
ghostMoveOne :: Ghost -> Ghost
ghostMoveOne g | gdirection g == GoUp = g {gpoint = (a, b + 1)}
               | gdirection g == GoRight = g {gpoint = (a + 1, b)}
               | gdirection g == GoLeft = g {gpoint = (a - 1, b)}
               | gdirection g == GoDown = g {gpoint = (a, b - 1)}
                where (a,b) = gpoint g


-- checks if a ghost can change direction
canChangeDirection :: GameState -> Ghost -> Bool
canChangeDirection gstate g | gdirection g == GoUp || gdirection g == GoDown = checkDirectionRightLeft (maze gstate) (gpoint g)
                            | gdirection g == GoLeft || gdirection g == GoRight = checkDirectionUpDown (maze gstate) (gpoint g)

--chooses which ghost algorithm is required
chooseGhost :: GameState -> Ghost -> Ghost
chooseGhost gstate g | ghosttype g == Blinky = choosePath gstate g (stepBlinky gstate g)
                     | ghosttype g == Inky = choosePath gstate g (stepInky gstate g)
                     | ghosttype g == Pinky = choosePath gstate g (stepPinky gstate g)
                     | ghosttype g == Clyde = choosePath gstate g (stepClyde gstate g)

-- help function for canChangeDirection
checkDirectionUpDown :: Maze -> Loadlevel.Point -> Bool
checkDirectionUpDown m p | t1 /= Barrier && t1 /= Wall = True
                         | t2 /= Barrier && t2 /= Wall = True
                         | otherwise = False
                        where ((a, b), t1) = searchMaze m (fst p, snd p + 1)
                              ((c, d), t2) = searchMaze m (fst p, snd p - 1)

-- help function for canChangeDirection
checkDirectionRightLeft :: Maze -> Loadlevel.Point -> Bool
checkDirectionRightLeft m p | t1 /= Barrier && t1 /= Wall = True
                            | t2 /= Barrier && t2 /= Wall = True
                            | otherwise = False
                           where ((a, b), t1) = searchMaze m (fst p + 1, snd p)
                                 ((c, d), t2) = searchMaze m (fst p - 1, snd p)

-- chooses target tile
stepBlinky :: GameState -> Ghost -> Loadlevel.Point
stepBlinky gstate g | mode g == Chase = (x,y)
                    | mode g == Scatter = (26, 0) -- top right
                    | otherwise = (x,y) -- moet nog veranderd worden
                    where (x,y) = point (pacman gstate)



stepInky :: GameState -> Ghost -> Loadlevel.Point
stepInky gstate g | mode g == Chase = undefined -- bottom right
                  | mode g == Scatter = (26, 30)

stepPinky :: GameState -> Ghost -> Loadlevel.Point
stepPinky = undefined -- top left   

stepClyde :: GameState -> Ghost -> Loadlevel.Point
stepClyde = undefined -- bottom left


-- determines which turn to make
choosePath :: GameState -> Ghost -> Loadlevel.Point -> Ghost
choosePath gstate g (x,y) = g {gdirection = direct}
              where list | gdirection g == GoUp = filter (filtWall (maze gstate)) [((a, b+1), (x,y), GoUp), ((a+1, b),(x,y), GoRight), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoDown = filter (filtWall (maze gstate)) [((a, b-1), (x,y), GoDown), ((a+1, b),(x,y), GoRight), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoLeft = filter (filtWall (maze gstate)) [((a, b+1), (x,y), GoUp), ((a, b-1),(x,y), GoDown), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoRight = filter (filtWall (maze gstate)) [((a, b+1), (x,y), GoUp), ((a, b-1), (x,y), GoDown), ((a+1, b), (x,y), GoRight)]
                    listfilt = map  calcDist list
                    min = minimum listfilt
                    ind = findIndex listfilt min
                    ((v,h),(j,k),direct) = list!!ind

                    (a,b) = gpoint g


findIndex :: [Float] -> Float -> Int
findIndex [] x = 0
findIndex (y:ys) x | x == y = 0
                   | otherwise = 1 + findIndex ys x 

-- calculates distance to target tile
calcDist :: (Loadlevel.Point, Loadlevel.Point, Loadlevel.Direction) -> Float
calcDist ((x1 , y1), (x2 , y2), d) = sqrt (x'*x' + y'*y')
    where
      x' = fromIntegral (x1 - x2)
      y' = fromIntegral (y1 - y2)

-- filters directions a ghost cant go in
filtWall :: Maze -> (Loadlevel.Point, Loadlevel.Point, Loadlevel.Direction) -> Bool
filtWall m ((a,b), (c,d), e) | t /= Wall && t /= Barrier = True
                             | otherwise = False
                   where ((x,y), t) = searchMaze m (a,b)