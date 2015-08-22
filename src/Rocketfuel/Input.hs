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

data MouseStatus = Clicked | Released
data Click = Click { _xy :: (Double, Double),
                     _status :: MouseStatus }

-- Main updater

updateContext :: Click -> GameContext -> GameContext
updateContext (Click (x,y) Clicked) gc = dragIfNeeded x y gc
updateContext (Click (x,y) Released) gc = dropIfNeeded x y gc

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
dragIfNeeded :: Double -> Double -> GameContext -> GameContext
dragIfNeeded x y g@(GameContext _ _ Nothing)
    = if uncurry legit coords
            then g { command = Just $ DragAndDrop (Just coords) Nothing }
            else g
    where coords = cellFromCoord x y
dragIfNeeded _ _ g = g

dropIfNeeded :: Double -> Double -> GameContext -> GameContext
dropIfNeeded x y g@(GameContext _ _ (Just (DragAndDrop (Just p) Nothing)))
    = if uncurry legit coords
        then execute $ g { command = Just $ DragAndDrop (Just p) (Just coords) }
        else g { command = Just $ DragAndDrop Nothing Nothing }
    where coords = cellFromCoord x y
dropIfNeeded _ _ g = g

-- Given a x, y index, check it can be a position on the grid.
legit :: (Num a, Ord a) => a -> a -> Bool
legit x y = x < 8 && x >= 0 && y < 8 && y >= 0

-- Given a global mouse input expressed in gloss viewport,
-- convert it to a 0,0 coord system on the grid.
cellFromCoord :: Double -> Double -> (Integer, Integer)
cellFromCoord x y = (round (globalX / 32.0), abs $ round (globalY / 32.0))
    where globalX = x
          globalY = y
