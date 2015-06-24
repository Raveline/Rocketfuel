module Rocketfuel.DisplayTypes (
    DragDropState,
    GameContext(..)
) where

import Rocketfuel.Types
import Graphics.Gloss

type DragDropState = Maybe Position
data GameContext = GameContext { _resources :: [Picture],
                                 grid :: Grid,
                                 dragDropState :: DragDropState }
