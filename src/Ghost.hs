module Ghost where


data Ghost = G1 Point Direction Mode | G2 Point Direction Mode | G3 Point Direction Mode | G4 Direction Mode Mode

data Mode = Frightened | Scatter | Chase

GhostMove :: Ghost -> Player -> PlayField -> Ghost
GhostMove g@(a p d m) _ _ _ | m == Frightened = undefined
                            | m == scatter = undefined
                            | m == Frightened = undefined

ScatterMove :: Ghost -> PlayField -> Ghost
ScatterMove g@(a p d m) pf@() |
                              |
                              |


