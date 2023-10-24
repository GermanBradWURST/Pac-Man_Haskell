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
            scoreList = drop 9 (images gstate)
            pacpic = translatePacMan (scaledlist!!3) (pacman gstate)
            blinky = translateGhost (scaledlist!!4) ((ghosts gstate)!!0)
            inky = translateGhost (scaledlist!!5) ((ghosts gstate)!!1)
            pinky = translateGhost (scaledlist!!7) ((ghosts gstate)!!2)
            clyde = translateGhost (scaledlist!!6) ((ghosts gstate)!!3)
            pellets = map (translatePellet (scaledlist!!1)) (pelletList (concat (maze gstate)))
            superPellets = map (translatePellet (scaledlist!!2)) (superPelletList (concat (maze gstate)))
            scoreText = [translateScoreText ((images gstate)!! 8)]
            score =  [translateScore (scoreList!!(intList!!0)) 10 , translateScore (scoreList!!(intList!!1)) 25 , translateScore (scoreList!!(intList!!2)) 40 , translateScore (scoreList!!(intList!!3)) 55 ]
                where 
                    intList = calculateScore gstate
        pictures ([scaledlist!!0] ++ pellets ++ superPellets ++ [pacpic, blinky, inky, pinky, clyde] ++ scoreText ++ score)


calculateScore :: GameState -> [Int]
calculateScore gstate = intToList (score gstate)
    where 
        intToList :: Int -> [Int]
        intToList 0 = [0,0,0,0]
        intToList n 
            | n < 100 = [0, 0, n `div` 10, n `mod` 10]
            | n < 1000 = [0, n `div` 100, (n `div` 10) `mod` 10,n `mod` 10]
            | otherwise = [n `div` 1000, (n `div` 100) `mod` 10, (n `div` 10) `mod` 10, n `mod` 10]
         

translateScoreText :: Picture -> Picture
translateScoreText pict = translate (-60) (270) pict

translateScore :: Picture -> Float -> Picture
translateScore pict offset = translate (-10 + offset) (270) pict

translatePellet :: Picture -> Tile -> Picture
translatePellet pict tile = translate (x*16-216) (240-(y*16)) pict
            where
                xs = fst (fst tile)
                ys = snd (fst tile)
                pos = (xs,ys)
                x = (fromIntegral . fst) pos
                y = (fromIntegral . snd) pos


                
pelletList :: [Tile] -> [Tile]
pelletList = filter (\tile -> snd tile == Pellet)

superPelletList :: [Tile] -> [Tile]
superPelletList = filter(\tile -> snd tile == SuperPellet)


translatePacMan :: Picture -> PacMan -> Picture
translatePacMan pict (PacMan (xs, ys) direc) = translate (x*16-216) (240-(y*16)) d
            where
                    pos = (xs,ys)
                    x = (fromIntegral . fst) pos
                    y = (fromIntegral . snd) pos
                    dp = direc
                    d | dp == GoUp = Rotate (-90) pict
                      | dp == GoRight = pict
                      | dp == GoLeft = Rotate 180 pict
                      | dp == GoDown = Rotate 90 pict
                      | otherwise = pict


translateGhost :: Picture -> Ghost -> Picture
translateGhost pict (Ghost gtype mode (xs, ys) direc time) = translate (x*16-216) (240-(y*16)) d
            where
                    pos = (xs,ys)
                    x = (fromIntegral . fst) pos
                    y = (fromIntegral . snd) pos
                    dp = direc
                    d   | dp == GoLeft = rotate 180 pict
                        | otherwise = pict


