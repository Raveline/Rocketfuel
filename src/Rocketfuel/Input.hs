module Rocketfuel.Input (
    dragIfNeeded,
    dropIfNeeded
) where

import Debug.Trace
import Rocketfuel.Types
import Rocketfuel.DisplayTypes
import Rocketfuel.Grid

-- Called when the mouse is down.
-- If there is no given tile being dragged,
-- given a mouse input expressed in x and y,
-- check if it fits in the grid; if so, start
-- dragging this tile by modifying GameContext.
dragIfNeeded :: Float -> Float -> GameContext -> GameContext
dragIfNeeded x y g@(GameContext _ _ Nothing)
    = if uncurry legit coords
            then g { command = Just $ DragAndDrop (Just coords) Nothing }
            else g
    where coords = cellFromCoord x y
modifyIfNeeded _ _ g = g

dropIfNeeded :: Float -> Float -> GameContext -> GameContext
dropIfNeeded x y g@(GameContext _ grid (Just (DragAndDrop (Just p) Nothing)))
    = if uncurry legit coords
        then execute $ g { command = Just $ DragAndDrop (Just p) (Just coords) }
        else g { command = Just $ DragAndDrop Nothing Nothing }
    where coords = cellFromCoord x y
dropIfNeeded _ _ g = g

-- Given a x, y index, check it can be a position on the grid.
legit x y = x < 8 && x >= 0 && y < 8 && y >= 0

-- Given a global mouse input expressed in gloss viewport,
-- convert it to a 0,0 coord system on the grid.
cellFromCoord :: Float -> Float -> (Integer, Integer)
cellFromCoord x y = (round (globalX / 32.0), abs $ round (globalY / 32.0))
    where globalX = x
          globalY = y
