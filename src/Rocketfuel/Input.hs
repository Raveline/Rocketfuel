{-# LANGUAGE PackageImports #-}
module Rocketfuel.Input (
    keyIsPressed,
    readMouse,
    Click (..),
    MouseStatus (..),
    updateContext
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad

import Rocketfuel.DisplayTypes

type Coords = (Integer, Integer)
data MouseStatus = Clicked | Released
data Click = Click { _xy :: (Double, Double),
                     _status :: MouseStatus }

-- Main updater

updateContext :: Click -> GameContext -> GameContext
updateContext (Click (x, y) status) gc = 
    if legit coords then updateContext' status
                    else gc
    where 
        coords = cellFromCoord x y
        updateContext' :: MouseStatus -> GameContext
        updateContext' Clicked = dragIfNeeded coords gc
        updateContext' Released = dropIfNeeded coords gc

-- Signal input management
--
readMouse :: Window -> (Click -> IO b) -> IO b
readMouse window sink = do
    pollEvents
    mousePos <- getCursorPos window
    mouseIsPressed <- checkMouseButtonStatus window MouseButtonState'Pressed
    if mouseIsPressed then sink $ Click mousePos Clicked
                      else sink $ Click mousePos Released

checkMouseButtonStatus :: Window -> MouseButtonState -> IO Bool
checkMouseButtonStatus win st = liftM (st ==) (getMouseButton win MouseButton'1)

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

-- Logic for drag'n'drop

-- Called when the mouse is down.
-- If there is no given tile being dragged,
-- given a mouse input expressed in x and y,
-- check if it fits in the grid; if so, start
-- dragging this tile by modifying GameContext.
dragIfNeeded :: Coords -> GameContext -> GameContext
dragIfNeeded coords g@(GameContext _ _ Nothing)
    = g { command = Just $ DragAndDrop (Just coords) Nothing }
dragIfNeeded _ g = g

dropIfNeeded :: Coords -> GameContext -> GameContext
dropIfNeeded coords g@(GameContext _ _ (Just (DragAndDrop (Just p) Nothing)))
    = g { command = Just $ DragAndDrop (Just p) (Just coords) }
dropIfNeeded _ g = g

-- Given a x, y index, check it can be a position on the grid.
legit :: (Num a, Ord a) => (a, a) -> Bool
legit (x, y) = x < 8 && x >= 0 && y < 8 && y >= 0

-- Given a global mouse input expressed in gloss viewport,
-- convert it to a 0,0 coord system on the grid.
cellFromCoord :: Double -> Double -> Coords
cellFromCoord x y = (floor (x / 32.0), abs $ floor (y / 32.0))
