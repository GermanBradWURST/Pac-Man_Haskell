module View where

import Model
import PacMan
import Loadlevel
import Ghosts

import Graphics.Gloss

view :: GameState -> IO Picture
view = return . viewPure

-- everything that can be seen on screen
viewPure :: GameState -> Picture
viewPure gstate = case viewState gstate of
    Running -> do
        let scaledlist = map (Scale 2.0 2.0) (images gstate)
            scoreList = map (Scale 2.0 2.0) (drop 9 (images gstate))
            -- pacpic = if ((elapsedTime gstate) % 2) == 0 then translatePacMan (scaledlist!!3) (pacman gstate) else  translatePacMan (scaledlist!!22) (pacman gstate)
            frame = (toInt ((elapsedTime gstate) * 24)) `mod` 8
            pacpic
                | frame `elem` [0, 1, 2] = translatePacMan (scaledlist !! 3) (pacman gstate)  -- Closed mouth
                | frame `elem` [3, 4, 5] = translatePacMan (scaledlist !! 24) (pacman gstate) -- Open mouth
                | otherwise = translatePacMan (scaledlist !! 23) (pacman gstate)              -- Closed mouth (default)
            blinky = if (mode ((ghosts gstate)!!0)) /= Frightened then translateGhost (scaledlist!!4) ((ghosts gstate)!!0) else translateGhost (scaledlist!!19) ((ghosts gstate)!!0)
            inky = if (mode ((ghosts gstate)!!1)) /= Frightened then translateGhost (scaledlist!!5) ((ghosts gstate)!!1) else translateGhost (scaledlist!!19) ((ghosts gstate)!!1)
            pinky = if (mode ((ghosts gstate)!!2)) /= Frightened then translateGhost (scaledlist!!7) ((ghosts gstate)!!2) else translateGhost (scaledlist!!19) ((ghosts gstate)!!2)
            clyde = if (mode((ghosts gstate)!!3)) /= Frightened then translateGhost (scaledlist!!6) ((ghosts gstate)!!3) else translateGhost (scaledlist!!19) ((ghosts gstate)!!3)
            pellets = map (translatePellet (scaledlist!!1)) (pelletList (concat (maze gstate)))
            superPellets = map (translatePellet (scaledlist!!2)) (superPelletList (concat (maze gstate)))
            scoreText = [translateText (scaledlist!! 8) (-60) 270 ]
            pacmanlives = translatePacmanLives (scaledlist!!3) (amountToListLives (lives gstate))
            score = [translateScore (scoreList!!(intList!!0)) 10 270 , translateScore (scoreList!!(intList!!1)) 25 270, translateScore (scoreList!!(intList!!2)) 40 270, translateScore (scoreList!!(intList!!3)) 55 270 ]  
                where 
                    intList = calculateScore gstate
        pictures (score ++ [scaledlist!!0] ++ pellets ++ superPellets ++ [pacpic, blinky, inky, pinky, clyde] ++ scoreText ++ pacmanlives)
    Paused -> do
        let scaledPaccy =  map (Scale 5.0 5.0) ([(images gstate)!!3])
            scaledText = map (Scale 2.0 2.0 ) ([(images gstate)!!20])
            pacPause = translatePacMan (scaledPaccy!!0) (PacMan (14,16) GoRight)
            pauseText = translateText (scaledText!!0) 0 60
        pictures ([pacPause] ++ [pauseText])
    
    GameOver -> do
        let scaledText = map (Scale 2.0 2.0) ([(images gstate)!!21] ++ [(images gstate)!!8])
            scoreList = map (Scale 2.0 2.0 ) (drop 9 (images gstate))
            gameover = translateText (scaledText!!0) 0 (20)
            scoreText = [translateText (scaledText!!1) (-80) (-50) ]
            score = [translateScore (scoreList!!(intList!!0)) 10 (-50) , translateScore (scoreList!!(intList!!1)) 35 (-50), translateScore (scoreList!!(intList!!2)) 30 (-50), translateScore (scoreList!!(intList!!3)) 45 (-50) ]
                where 
                    intList = calculateScore gstate 
        pictures ([gameover]++score++scoreText)

    Ready -> do
        let scaledlist = map (Scale 2.0 2.0) (images gstate)
            glist = [translatePicture (scaledlist!!4) (13.5, 11), translatePicture (scaledlist!!5) (11.5, 14), translatePicture (scaledlist!!6) (13.5, 14), translatePicture (scaledlist!!7) (15.5, 14)]
            pellets = map (translatePellet (scaledlist!!1)) (pelletList (concat (maze gstate)))
            superPellets = map (translatePellet (scaledlist!!2)) (superPelletList (concat (maze gstate)))
            scoreList = map (Scale 2.0 2.0) (drop 9 (images gstate))
            scoreText = [translateText (scaledlist!! 8) (-60) 270 ]
            score = [translateScore (scoreList!!(intList!!0)) 10 270 , translateScore (scoreList!!(intList!!1)) 25 270, translateScore (scoreList!!(intList!!2)) 40 270, translateScore (scoreList!!(intList!!3)) 55 270 ]  
                where 
                    intList = calculateScore gstate
            pacmanlives = translatePacmanLives (scaledlist!!3) (amountToListLives (lives gstate))

        pictures ([head scaledlist, translatePicture (scaledlist!!3) (13.5, 23), translatePicture (scaledlist!!22) (13.5, 17)] ++ glist ++ pellets ++ superPellets ++ scoreText ++ score ++ pacmanlives)

    Dying -> do
        let scaledlist = map (Scale 2.0 2.0) (images gstate)
            scoreList = map (Scale 2.0 2.0) (drop 9 (images gstate))
            frame = (toInt ((elapsedTime gstate) * 24)) `mod` 8
            dyingList = drop 25 scaledlist
            timer = deadCounter gstate
            currentIndex | timer > 103 = 11
                         | timer > 70 = floor ( fromIntegral(timer - 70) / 3.0)
            pacpic = translatePacMan (dyingList!!currentIndex) (pacman gstate)
            pellets = map (translatePellet (scaledlist!!1)) (pelletList (concat (maze gstate)))
            superPellets = map (translatePellet (scaledlist!!2)) (superPelletList (concat (maze gstate)))
            scoreText = [translateText (scaledlist!! 8) (-60) 270 ]
            pacmanlives = translatePacmanLives (scaledlist!!3) (amountToListLives (lives gstate))
            score = [translateScore (scoreList!!(intList!!0)) 10 270 , translateScore (scoreList!!(intList!!1)) 25 270, translateScore (scoreList!!(intList!!2)) 40 270, translateScore (scoreList!!(intList!!3)) 55 270 ]  
                where 
                    intList = calculateScore gstate
        pictures (score ++ [scaledlist!!0] ++ pellets ++ superPellets ++ [pacpic] ++ scoreText ++ pacmanlives)
        



calculateScore :: GameState -> [Int]
calculateScore gstate = intToList (score gstate)
    where 
        intToList :: Int -> [Int]
        intToList 0 = [0,0,0,0]
        intToList n 
            | n < 100 = [0, 0, n `div` 10, n `mod` 10]
            | n < 1000 = [0, n `div` 100, (n `div` 10) `mod` 10,n `mod` 10]
            | otherwise = [n `div` 1000, (n `div` 100) `mod` 10, (n `div` 10) `mod` 10, n `mod` 10]
         
     
toInt :: Float -> Int 
toInt f = round f


translateText :: Picture -> Float -> Float -> Picture
translateText pict xoffset yoffset = translate (xoffset) (yoffset) pict

translateScore :: Picture -> Float -> Float -> Picture
translateScore pict xoffset yoffset = translate (-10 + xoffset) (yoffset) pict

translatePellet :: Picture -> Tile -> Picture
translatePellet pict tile = translate (x*16-216) (240-(y*16)) pict
            where
                xs = fst (fst tile)
                ys = snd (fst tile)
                pos = (xs,ys)
                x =  fst pos
                y =  snd pos


                
pelletList :: [Tile] -> [Tile]
pelletList = filter (\tile -> snd tile == Pellet)

superPelletList :: [Tile] -> [Tile]
superPelletList = filter(\tile -> snd tile == SuperPellet)


translatePacMan :: Picture -> PacMan -> Picture
translatePacMan pict (PacMan (xs, ys) direc) = translate (x*16-216) (240-(y*16)) d
            where
                    pos = (xs,ys)
                    x =  fst pos
                    y =  snd pos
                    dp = direc
                    d | dp == GoUp = Rotate (-90) pict
                      | dp == GoRight = pict
                      | dp == GoLeft = Rotate 180 pict
                      | dp == GoDown = Rotate 90 pict
                      | otherwise = pict



translatePacmanLives :: Picture -> [Int] -> [Picture]
translatePacmanLives _ [] = []
translatePacmanLives pict (x:xs) = translate ( (-200) + (30 * (fromIntegral (length xs)))) (-270) pict : translatePacmanLives pict xs
                      


amountToListLives :: Int -> [Int] 
amountToListLives i
    |i == 3 = [1,1,1]
    |i == 2 = [1,1]
    |i == 1 = [1]
    |otherwise = []



translateGhost :: Picture -> Ghost -> Picture
translateGhost pict (Ghost gtype mode (xs, ys) direc time speed tt b) = translate (x*16-216) (240-(y*16)) d
            where
                    pos = (xs,ys)
                    x =  fst pos
                    y =  snd pos
                    dp = direc
                    d   | dp == GoLeft = scale (-1) (1) pict
                        | otherwise = pict


translatePicture :: Picture -> (Float, Float) -> Picture
translatePicture pict (x,y) = translate (x*16-216) (240-(y*16)) pict

