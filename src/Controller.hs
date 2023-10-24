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

generateRandomNum :: (Int, Int) -> Int
generateRandomNum (minn, maxx) = fst $ randomR (minn, maxx) iRNG

iRNG :: StdGen
iRNG = mkStdGen 2231542

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do
    let pman = stepPacMan gstate
    let glist = map (stepGhost gstate) (ghosts gstate)
    let pelletmaze = [[eatPellet tile pman | tile <- row] | row <- maze gstate] -- checking if the current pacman is eating a pellet
    let newmaze = [[eatSuperPellet tile pman | tile <- row] | row <- pelletmaze] -- checking if the current pacman is eating a super pellet
    let newScore = 10  * (260 - (calculateScore (concat(maze gstate))))
    let ghostsMode = changeGhostMode (scoreChange (score gstate) newScore) (glist)
    putStrLn (show (mode (ghostsMode!!0)))
        
    return $ gstate { elapsedTime = elapsedTime gstate + secs, pacman = pman, maze = newmaze, ghosts = ghostsMode, score = newScore}

stepPacMan :: GameState -> PacMan
stepPacMan gstate | checkWrap (point (pacman gstate)) (direction (pacman gstate)) = wrapAroundPacMan (pacman gstate)
                  | canMove (pacman gstate) (maze gstate) = movePacMan (pacman gstate) gstate
                  | otherwise = pacman gstate


-- handles pacman eating pellets (and keeping score)
eatPellet :: Tile -> PacMan -> Tile
eatPellet tile (PacMan (x,y) d) = if ((fst tile) == (x,y)) && (snd tile) == Pellet then ((fst tile), Empty) else tile    

-- handles pacman eating super pellets
eatSuperPellet :: Tile -> PacMan -> Tile
eatSuperPellet tile (PacMan (x,y) d) = if ((fst tile) == (x,y)) && (snd tile) == SuperPellet then ((fst tile), Empty) else tile


-- takes all the tiles and calculates how many of them contain a pellet, or a superpellet
calculateScore :: [Tile] -> Int
calculateScore tiles  = (length (pelletList tiles))  + (5 * (length (superPelletList tiles)))
    where
        pelletList :: [Tile] -> [Tile]
        pelletList = filter (\tile -> snd tile == Pellet)
        superPelletList :: [Tile] -> [Tile] 
        superPelletList = filter (\tile -> snd tile == SuperPellet) 


scoreChange :: Int -> Int -> Bool
scoreChange oldscore newscore = abs (oldscore - newscore) > 10 

changeGhostMode :: Bool -> [Ghost] -> [Ghost]
changeGhostMode pred ghosts
    | pred = map toFrightened ghosts  
    | ((gTimer fstGhost) `mod` 15 == 0) && ((mode fstGhost) == Frightened) = map toScatter ghosts
    | otherwise = ghosts
    where
        fstGhost = ghosts!!0
        
-- change the mode of the ghosts to frightened
toFrightened :: Ghost -> Ghost
toFrightened g = g { mode = Frightened }

-- change the mode of the ghosts to scatter
toScatter :: Ghost -> Ghost
toScatter g = g {mode = Scatter, gTimer = 0}

-- change the mode of the ghosts to chase
toChase :: Ghost -> Ghost
toChase g = g {mode = Chase, gTimer = 0 }


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
stepGhost gstate g | canChangeDirection gstate g = (ghostMoveOne (chooseGhost gstate g)) {gTimer = newTimer}
                   | checkWrap (gpoint g) (gdirection g) = wrapAroundGhost g
                   | otherwise = ghostMoveOne g
                        where
                            newTimer = (gTimer g) + 1


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
                    | mode g == Scatter = (26, 0) 
                    | otherwise = (xx, yy)
                    where (x,y) = point (pacman gstate)
                          (a, b) = gpoint g
                          xx = generateRandomNum (a - 10, a + 10)
                          yy = generateRandomNum (b - 10, b + 10)




stepInky :: GameState -> Ghost -> Loadlevel.Point
stepInky gstate g | mode g == Chase = (ga, gb)
                  | mode g == Scatter = (26, 30)
                  | otherwise = (xx, yy)
         where (x,y) = point (pacman gstate)
               (gx, gy) = gpoint ((ghosts gstate)!!0)
               dir = direction (pacman gstate)
               (a,b) | dir == GoUp = (x - 2, y - 2)
                     | dir == GoDown = (x, y + 2)
                     | dir == GoLeft = (x - 2, y)
                     | dir == GoRight = (x + 2, y)
               (ga, gb) = (a + (a - gx), b + (b - gy))
               xx = generateRandomNum (a - 10, a + 10)
               yy = generateRandomNum (b - 10, b + 10)
               
                        

stepPinky :: GameState -> Ghost -> Loadlevel.Point
stepPinky gstate g | mode g == Chase = (a, b)
                   | mode g == Scatter = (1, 0)
                   | otherwise = (xx, yy)
         where (x,y) = point (pacman gstate)
               dir = direction (pacman gstate)
               (a,b) | dir == GoUp = (x - 4, y - 4)
                     | dir == GoDown = (x, y + 4)
                     | dir == GoLeft = (x - 4, y)
                     | dir == GoRight = (x + 4, y)
               xx = generateRandomNum (a - 10, a + 10)
               yy = generateRandomNum (b - 10, b + 10)


stepClyde :: GameState -> Ghost -> Loadlevel.Point
stepClyde gstate g | mode g == Chase = (gx, gy)
                   | mode g == Scatter = (1, 30)
                   | otherwise = (xx, yy)
         where (x,y) = point (pacman gstate)
               (a, b) = gpoint g
               dis = calcDist ((x,y), (a,b), GoRight)
               (gx, gy) | dis > 8 = (x, y)
                        | otherwise = (1, 30)
               xx = generateRandomNum (a - 10, a + 10)
               yy = generateRandomNum (b - 10, b + 10)

               


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


