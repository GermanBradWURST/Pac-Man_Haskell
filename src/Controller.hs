module Controller where

import Model
import Ghosts
import PacMan
import Loadlevel

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Concurrent (dupChan)


searchMaze :: Maze -> (Float, Float) -> Tile
searchMaze m (x, y) = (m!!inty)!!intx 
    where
        inty = toInt y 
        intx = toInt x

--Checks for wrap around
checkWrap :: (Float, Float) -> Direction -> Bool
checkWrap p d  | isClose p (0,14) 0.1 && d == GoLeft = True
               | isClose p (27, 14) 0.1 && d == GoRight = True
               | otherwise = False
                   

generateRandomNum :: (Int, Int) -> Int
generateRandomNum (minn, maxx) = fst $ randomR (minn, maxx) iRNG

iRNG :: StdGen
iRNG = mkStdGen 2231542

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    | (viewState gstate) == Running = do
        let isFrightened = checkGhostFrightened (ghosts gstate)
        let ghostsl = teleportGhost gstate
        let pman = stepPacMan gstate
        let glist = map (stepGhost gstate) (ghostsl)
        let pelletmaze = [[eatPellet tile pman | tile <- row] | row <- maze gstate] -- checking if the current pacman is eating a pellet
        let newmaze = [[eatSuperPellet tile pman | tile <- row] | row <- pelletmaze] -- checking if the current pacman is eating a super pellet
        let newScore = 10  * (260 - (calculateScore (concat(maze gstate))))
        let ghostsMode = changeGhostMode (ghostTimer gstate) isFrightened (scoreChange (score gstate) newScore) (glist)
        let newlives = lives (collisionGhostPacman pman ghostsMode gstate)
        let newGhostTimer = upDateGhostTime isFrightened (ghostTimer gstate) secs
        let possibleGameOver = viewState (gameOver gstate)
        let possibleEatenGhost = map (collisionPacmanGhost pman gstate) (ghostsMode) 
        --putStrLn (show (mode (ghostsMode!!0)))
        --putStrLn (show (viewState gstate))
        -- putStrLn (show (lives gstate))
        if newlives < lives gstate
            then return $ resetGame gstate
            else return $ gstate { viewState = possibleGameOver, elapsedTime = elapsedTime gstate + secs, pacman = pman, maze = newmaze, ghosts = possibleEatenGhost, score = newScore, ghostTimer = newGhostTimer, lives = newlives}
    | (viewState gstate) == Paused = do
        return $ gstate


    | (viewState gstate) == Ready = do
        let lst = lastPressed gstate
            start | lst == 'a' || lst == 's' || lst == 'w' || lst == 'd' = True
                  | otherwise = False
        if start == True
            then return $ gstate {viewState = Running}
            else return $ gstate


    | (viewState gstate) == GameOver = do
        levelfile <- readFile "src/Level1.txt"
        let level = lines levelfile
        let maze = makeMaze level
        let lst = lastPressed gstate
        let blinky = Ghost Blinky Scatter (13, 11) GoRight 1 0.25 0 False
        let inky = Ghost Inky Scatter (13, 14) GoRight 1 0.25 0 True
        let clyde = Ghost Clyde Scatter (13, 14) GoRight 1 0.25 0 True
        let pinky = Ghost Pinky Scatter (13, 14) GoRight 1 0.25 0 True
        let ghosts = [blinky, inky, pinky, clyde]
        let start | lst == 'c' = True
                  | otherwise = False

        let initial = Model.initialState initialPacMan (images gstate) maze ghosts
        if start == True
            then return $ initial
            else return $ gstate


    | otherwise = return $ gstate

stepPacMan :: GameState -> PacMan
stepPacMan gstate | checkWrap (point (pacman gstate)) (direction (pacman gstate)) = wrapAroundPacMan (pacman gstate)
                  | canMove (pacman gstate) (maze gstate) = movePacMan (pacman gstate) gstate
                  | otherwise = pacman gstate


-- handles pacman eating pellets (and keeping score)
eatPellet :: Tile -> PacMan -> Tile
eatPellet tile (PacMan (x,y) d) = if isClose (x,y) (fst tile) 0.3 && (snd tile) == Pellet then ((fst tile), Empty) else tile

-- handles pacman eating super pellets
eatSuperPellet :: Tile -> PacMan -> Tile
eatSuperPellet tile (PacMan (x,y) d) = if isClose (x,y) (fst tile) 0.3 && (snd tile) == SuperPellet then ((fst tile), Empty) else tile

-- seeing if two points are in a given range off each other
isClose :: (Float, Float) -> (Float, Float) -> Float -> Bool
isClose (px, py) (tx, ty) range = sqrt ((px - tx) ^ 2 + (py - ty) ^ 2) <= range

-- takes all the tiles and calculates how many of them contain a pellet, or a superpellet
calculateScore :: [Tile] -> Int
calculateScore tiles  = (length (pelletList tiles))  + (5 * (length (superPelletList tiles)))
    where
        pelletList :: [Tile] -> [Tile]
        pelletList = filter (\tile -> snd tile == Pellet)
        superPelletList :: [Tile] -> [Tile] 
        superPelletList = filter (\tile -> snd tile == SuperPellet) 

-- if the score changes with more than 10 points in a single step it means a power pellet has been eaten
scoreChange :: Int -> Int -> Bool
scoreChange oldscore newscore = abs (oldscore - newscore) > 10 

collisionGhostPacman :: PacMan -> [Ghost] -> GameState -> GameState
collisionGhostPacman pman [] gstate = gstate
collisionGhostPacman pman (x:xs) gstate
    | isClose (px, py) (gpoint x) 0.50 && (mode x) /= Frightened = loseLife gstate
    | otherwise = collisionGhostPacman pman xs gstate
            where
                (PacMan (px, py) pd) = pman

collisionPacmanGhost :: PacMan -> GameState -> Ghost -> Ghost
collisionPacmanGhost pman gstate g= if isClose (point pman) (gpoint g) 0.50 then toChase ( g {gpoint = (13,11), gTimer = 0} )else g
    where 
        newScore = eatingGhosts (ghosts gstate)
        
                
eatingGhosts :: [Ghost] -> Int
eatingGhosts [] = 0
eatingGhosts (x:xs)
    | (mode x) /= Frightened = calcEatGhostPoints (incr + 1) + eatingGhosts xs
    | otherwise = eatingGhosts xs
    where
        incr = 0

calcEatGhostPoints :: Int -> Int
calcEatGhostPoints i = 200 * (2^i)

loseLife :: GameState -> GameState
loseLife gstate = gstate {lives = decreaseLive}
    where
        decreaseLive = if (lives gstate) > 0 then (lives gstate) - 1 else (lives gstate)
        


-- resets the positions of pacman and the ghosts if pacman loses a life
resetGame :: GameState -> GameState
resetGame gstate | decreaseLife > 0 = gstate { pacman = initialPacMan, ghosts = initialGhosts, lives = decreaseLife, viewState = Ready, lastPressed = 'n'}
                 | otherwise = gstate { pacman = initialPacMan, ghosts = initialGhosts, lives = decreaseLife, viewState = GameOver, lastPressed = 'n'}
    where
        blinky = Ghost Blinky Scatter (13, 11) GoRight 1 0.25 0 False
        inky = Ghost Inky Scatter (13, 14) GoRight 1 0.25 0 True
        clyde = Ghost Clyde Scatter (13, 14) GoRight 1 0.25 0 True
        pinky = Ghost Pinky Scatter (13, 14) GoRight 1 0.25 0 True
        initialPacMan = PacMan (14, 23) GoRight
        initialGhosts = [blinky, inky, pinky, clyde]
        decreaseLife = if (lives gstate) > 0 then (lives gstate) - 1 else (lives gstate)


teleportGhost :: GameState -> [Ghost]
teleportGhost gstate | s >= 50 && inHouse pink = [blink, ink, pink {gpoint = (13, 11), inHouse = False}, clyd]
                     | s >= 300 && inHouse ink = [blink, ink {gpoint = (13,11), inHouse = False}, pink, clyd]
                     | s >= 900 && inHouse clyd = [blink, ink, pink, clyd {gpoint = (13,11), inHouse = False}]
                     | otherwise = glist
                 where glist@[blink,ink,pink,clyd] = ghosts gstate
                       s = score gstate

upDateGhostTime :: Bool -> Float -> Float -> Float
upDateGhostTime b f secs | not b = f + secs
                         | otherwise = f

checkGhostFrightened :: [Ghost] -> Bool
checkGhostFrightened [] = False
checkGhostFrightened (x:xs)  = mode x == Frightened || checkGhostFrightened xs

changeGhostMode :: Float -> Bool -> Bool -> [Ghost] -> [Ghost]
changeGhostMode gt isf pred ghosts
    | pred = map toFrightened ghosts  
    | ((gTimer fstGhost) `mod` 140 == 0) && ((mode fstGhost) == Frightened) = map toChase ghosts
    | not isf && gt > 84 = map toChase ghosts
    | not isf && gt > 79 = map toScatter ghosts
    | not isf && gt > 59 = map toChase ghosts
    | not isf && gt > 54 = map toScatter ghosts
    | not isf && gt > 34 = map toChase ghosts
    | not isf && gt > 27 = map toScatter ghosts
    | not isf && gt > 7 = map toChase ghosts
    | otherwise = ghosts
    where
        fstGhost = ghosts!!0
        
-- change the mode of the ghosts to frightened
toFrightened :: Ghost -> Ghost
toFrightened g = g { mode = Frightened, speed = 0.15 }

-- change the mode of the ghosts to scatter
toScatter :: Ghost -> Ghost
toScatter g = g {mode = Scatter, gTimer = 1, speed = 0.25}

-- change the mode of the ghosts to chase
toChase :: Ghost -> Ghost
toChase g = g {mode = Chase, gTimer = 1, speed = 0.25 }


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e  gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 's' && c /= lastPressed gstate && (viewState gstate) /= Paused = changeDirection GoDown gstate
    | c == 'w' && c /= lastPressed gstate && (viewState gstate) /= Paused  = changeDirection GoUp gstate
    | c == 'a' && c /= lastPressed gstate && (viewState gstate) /= Paused  = changeDirection GoLeft gstate
    | c == 'd' && c /= lastPressed gstate && (viewState gstate) /= Paused  = changeDirection GoRight gstate
    | c == 'p' = pauseGame gstate (lastPressed gstate) c
    | otherwise = gstate {lastPressed = c}
inputKey _ gstate = gstate



pauseGame :: GameState -> Char -> Char -> GameState
pauseGame gstate lastP c
    | c == 'p' && lastP == 'p' = gstate {viewState = Running, lastPressed = 'p'}
    | c=='p' && lastP /= 'p' = gstate {viewState = Paused, lastPressed = 'p'}
    |otherwise = gstate

gameOver :: GameState -> GameState
gameOver gstate 
    | (lives gstate) == 0 = gstate {viewState = GameOver}
    | otherwise = gstate


makeRound :: Float -> Float
makeRound f = fromIntegral (round f)


roundPoint :: (Float, Float) -> (Float, Float)
roundPoint p = (makeRound (fst p), makeRound (snd p))


--handling direction changes, pacman can't move in a certain direction if that would lead him 
--directly into a wall
changeDirection :: Direction -> GameState -> GameState
changeDirection direc gstate
    |(validMove (PacMan (x,y) pacdirec) direc m) = gstate {pacman = (PacMan (makeRound x,makeRound y) direc), lastPressed = direcToChar direc}
    |otherwise = gstate
        where
            direcToChar :: Direction -> Char
            direcToChar d
                |d == GoLeft = 'a'
                |d == GoRight = 'd'
                |d == GoUp = 'w'
                |d == GoDown ='s'
            (PacMan (x,y) pacdirec) = pacman gstate
            m = maze gstate


toInt :: Float -> Int 
toInt f = round f

--helper function to check validity of a move
validMove :: PacMan -> Direction -> Maze -> Bool
validMove (PacMan (x,y) pacdirec) direc maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        inty = toInt y
        intx = toInt x
        currentRow = maze !! inty
        rowUp = maze !! (inty-1)
        rowDown = maze !! (inty+1)
        nextTile    |direc == GoUp = rowUp !! intx
                    |direc == GoDown = rowDown !! intx
                    |direc == GoRight && intx < 27 = currentRow !! (intx + 1)
                    |direc == GoLeft && intx > 0= currentRow !! (intx - 1)
                    |otherwise = currentRow !! intx

--allowing pacman to move, which as of now is just teleporting 1 block
movePacMan :: PacMan -> GameState -> PacMan
movePacMan p@(PacMan (x,y) pacdirec) gstate
    |(pacdirec == GoUp) && canMove p (maze gstate) = (PacMan (x,y-0.25) pacdirec)
    |pacdirec == GoDown && canMove p (maze gstate) = (PacMan (x, y+0.25) pacdirec)
    |pacdirec == GoRight && canMove p (maze gstate) = (PacMan (x + 0.25,y) pacdirec)
    |pacdirec == GoLeft && canMove p (maze gstate) = (PacMan (x - 0.25, y) pacdirec)
    |otherwise = p


-- checking if pacman is able to move or if this moves him into a wall
canMove :: PacMan -> Maze -> Bool
canMove (PacMan (x,y) pacdirec) maze = (snd nextTile) /= Wall && (snd nextTile) /= Barrier
    where
        inty = toInt y
        intx = toInt x
        currentRow = maze !! inty
        rowUp = maze !! (inty-1)
        rowDown = maze !! (inty+1)
        nextTile    |pacdirec == GoUp = rowUp !! intx
                    |pacdirec == GoDown = rowDown !! intx
                    |pacdirec == GoRight && intx < 27 = currentRow !! (intx + 1)
                    |pacdirec == GoLeft && intx > 0= currentRow !! (intx - 1) 
                    |otherwise = currentRow !! intx

--Ghost step calcs
stepGhost :: GameState -> Ghost -> Ghost
stepGhost gstate g | turnTimer g == 0 && canChangeDirection gstate g = (ghostMoveOne (chooseGhost gstate newg)) {turnTimer = ttf}
                   | checkWrap (gpoint g) (gdirection g) = decreaseturnTimer (wrapAroundGhost newg)
                   | otherwise = decreaseturnTimer (ghostMoveOne newg)
                   where ttf | mode g == Frightened = 4
                             | otherwise = 2
                         newg = g {gTimer = incrementTimer g}
                      
decreaseturnTimer :: Ghost -> Ghost
decreaseturnTimer g | tt > 0 = g {turnTimer = (tt - 1)}
                    | otherwise = g
                where tt = turnTimer g
                           


-- icrementing the fright counter 
incrementTimer :: Ghost -> Int
incrementTimer ghost 
    | (mode ghost)== Frightened = newTimer
    | otherwise = gTimer ghost
        where
            newTimer = (gTimer ghost) + 1


--Moves the ghost one forward when it cant turn
ghostMoveOne :: Ghost -> Ghost
ghostMoveOne g | gdirection g == GoUp = g {gpoint = (makeRound a, b + s)}
               | gdirection g == GoRight = g {gpoint = (a + s, makeRound b)}
               | gdirection g == GoLeft = g {gpoint = (a - s, makeRound b)}
               | gdirection g == GoDown = g {gpoint = (makeRound a, b - s)}
                where 
                    (a,b) = gpoint g
                    s = speed g


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
                    | mode g == Scatter = (26.0, 0.0) 
                    | otherwise = (fromIntegral xx, fromIntegral yy)
                    where (x,y) = point (pacman gstate)
                          (a, b) = gpoint g
                          xx = generateRandomNum ((toInt a) - 10, (toInt a) + 10)
                          yy = generateRandomNum ((toInt b) - 10, (toInt b) + 10)




stepInky :: GameState -> Ghost -> Loadlevel.Point
stepInky gstate g | mode g == Chase = (ga, gb)
                  | mode g == Scatter = (26.0, 30.0)
                  | otherwise = (fromIntegral xx, fromIntegral yy)
         where (x,y) = point (pacman gstate)
               (gx, gy) = gpoint ((ghosts gstate)!!0)
               dir = direction (pacman gstate)
               (a,b) | dir == GoUp = (x - 2, y - 2)
                     | dir == GoDown = (x, y + 2)
                     | dir == GoLeft = (x - 2, y)
                     | dir == GoRight = (x + 2, y)
               (ga, gb) = (a + (a - gx), b + (b - gy))
               xx = generateRandomNum ((toInt a) - 10, (toInt a) + 10)
               yy = generateRandomNum ((toInt b) - 10, (toInt b) + 10)
               
                        

stepPinky :: GameState -> Ghost -> Loadlevel.Point
stepPinky gstate g | mode g == Chase = (a, b)
                   | mode g == Scatter = (1.0, 0.0)
                   | otherwise = (fromIntegral xx, fromIntegral yy)
         where (x,y) = point (pacman gstate)
               dir = direction (pacman gstate)
               (a,b) | dir == GoUp = (x - 4, y - 4)
                     | dir == GoDown = (x, y + 4)
                     | dir == GoLeft = (x - 4, y)
                     | dir == GoRight = (x + 4, y)
               xx = generateRandomNum ((toInt a) - 10, (toInt a) + 10)
               yy = generateRandomNum ((toInt b) - 10, (toInt b) + 10)


stepClyde :: GameState -> Ghost -> Loadlevel.Point
stepClyde gstate g | mode g == Chase = (gx, gy)
                   | mode g == Scatter = (1.0, 30.0)
                   | otherwise = (fromIntegral xx, fromIntegral yy)
         where (x,y) = point (pacman gstate)
               (a, b) = gpoint g
               dis = calcDist ((x,y), (a,b), GoRight)
               (gx, gy) | dis > 8 = (x, y)
                        | otherwise = (1, 30)
               xx = generateRandomNum ((toInt a) - 10, (toInt a) + 10)
               yy = generateRandomNum ((toInt b) - 10, (toInt b) + 10)

               





choosePath :: GameState -> Ghost -> Loadlevel.Point -> Ghost
choosePath gstate g (x,y) = g {gdirection = direct}
              where list | gdirection g == GoUp = filter (filtWall (maze gstate)) [((a, b +1), (x,y), GoUp), ((a+1, b),(x,y), GoRight), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoDown = filter (filtWall (maze gstate)) [((a, b-1), (x,y), GoDown), ((a+1, b),(x,y), GoRight), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoLeft = filter (filtWall (maze gstate)) [((a, b+1), (x,y), GoUp), ((a, b-1),(x,y), GoDown), ((a-1, b),(x,y), GoLeft)]
                         | gdirection g == GoRight = filter (filtWall (maze gstate)) [((a, b+1), (x,y), GoUp), ((a, b-1), (x,y), GoDown), ((a+1, b), (x,y), GoRight)]
                    listfilt = map calcDist list
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
      x' =  (x1 - x2)
      y' =  (y1 - y2)

-- filters directions a ghost cant go in
filtWall :: Maze -> (Loadlevel.Point, Loadlevel.Point, Loadlevel.Direction) -> Bool
filtWall m ((a,b), (c,d), e) | t /= Wall && t /= Barrier = True
                             | otherwise = False
                   where ((x,y), t) = searchMaze m (a,b)
