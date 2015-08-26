module Rocketfuel.Commands (
    execute
) where

import Rocketfuel.Types
import Rocketfuel.Grid

execute :: Maybe Command -> GameContext -> GameContext
execute com context = maybe context (execute' context) com
    where
        execute' gc (DragAndDrop (Just p1) (Just p2)) = swap p1 p2 gc
        execute' gc _ = gc

swap :: Position -> Position -> GameContext -> GameContext
swap p1 p2 gc@(GameContext g _) = if p1 /= p2 && orthoClose p1 p2 
                    then gc { grid = applySwap (Swap p1 p2) g }
                    else gc

orthoClose :: Position -> Position -> Bool
orthoClose (x,y) (x2,y2) = abs(x2-x) + abs(y2-y) == 1
