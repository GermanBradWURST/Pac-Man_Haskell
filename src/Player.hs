module Player where


type Point = (Int, Int)
data Direction = PUp | PDown | PLeft | PRight deriving (Eq)

data PacMan = MakePacMan {
    position :: Point
    , direction :: Direction
}


--handling movement of PacMan
changeDirection :: PacMan -> Direction -> PacMan
changeDirection (MakePacMan (x,y) pd) d = (MakePacMan (x,y) d)

movePacman :: PacMan -> PacMan
movePacman = undefined
                                