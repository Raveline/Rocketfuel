module Rocketfuel.DisplayTypes (
    Command (..),
    GameContext(..),
    execute
) where

import Graphics.Gloss

import Rocketfuel.Types
import Rocketfuel.Grid

data GameContext = GameContext { grid :: Grid,
                                 command :: Maybe Command }

data Command = DragAndDrop (Maybe Position) (Maybe Position)

execute :: GameContext -> GameContext
execute = removeCommand . execute'

removeCommand :: GameContext -> GameContext
removeCommand g = g { command = Nothing }

execute' :: GameContext -> GameContext
execute' gc@(GameContext g (Just (DragAndDrop (Just p1) (Just p2))))
    = if p1 /= p2 && orthoClose p1 p2 
        then gc { grid = applySwap (Swap p1 p2) g }
        else gc
execute' gc = gc

orthoClose :: Position -> Position -> Bool
orthoClose (x,y) (x2,y2) = abs(x2-x) + abs(y2-y) == 1
