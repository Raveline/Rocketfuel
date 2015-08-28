module Rocketfuel.Commands (
    runCommand,
    updateContext
) where

import Control.Monad
import Control.Monad.Random

import Rocketfuel.Types
import Rocketfuel.Grid

updateContext :: GameContext -> IO GameContext
updateContext context = do let baseGrid = grid context
                           (newGrid, effects) <- afterMove baseGrid
                           let finalContext = context { grid = newGrid }
                           return finalContext

runCommand :: Maybe Command -> GameContext -> GameContext
runCommand com context = maybe context (process' context) com
    where
        process' gc (DragAndDrop (Just p1) (Just p2)) = swap p1 p2 gc
        process' gc _ = gc

swap :: Position -> Position -> GameContext -> GameContext
swap p1 p2 gc@(GameContext g _) = if p1 /= p2 && orthoClose p1 p2 
                    then gc { grid = applySwap (Swap p1 p2) g }
                    else gc

orthoClose :: Position -> Position -> Bool
orthoClose (x,y) (x2,y2) = abs(x2-x) + abs(y2-y) == 1
