module Rocketfuel.Commands (
    execute
) where

import Rocketfuel.Types
import Rocketfuel.Grid

execute :: Command -> GameContext -> GameContext
execute (DragAndDrop (Just p1) (Just p2)) gc = swap p1 p2 gc
execute _ gc = gc

swap :: Position -> Position -> GameContext -> GameContext
swap p1 p2 gc@(GameContext g _) = if p1 /= p2 && orthoClose p1 p2 
                    then gc { grid = applySwap (Swap p1 p2) g }
                    else gc

orthoClose :: Position -> Position -> Bool
orthoClose (x,y) (x2,y2) = abs(x2-x) + abs(y2-y) == 1
