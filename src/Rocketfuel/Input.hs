{-# LANGUAGE PackageImports #-}
module Rocketfuel.Input (
    keyIsPressed,
    readMouse,
    Click (..),
    MouseStatus (..),
    updateCommand
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad
import Rocketfuel.Types

type Coords = (Integer, Integer)
data MouseStatus = Clicked | Released
data Click = Click { _xy :: (Double, Double),
                     _status :: MouseStatus }

-- Main updater

updateCommand :: Click -> Maybe Command -> Maybe Command
updateCommand click comm = readClick click . cleanCommand $ comm
    where 
        cleanCommand :: Maybe Command -> Maybe Command
        cleanCommand (Just (DragAndDrop (Just _) (Just _))) = Nothing
        cleanCommand other = other
        readClick :: Click -> Maybe Command -> Maybe Command
        readClick (Click (x, y) status) com = 
            if legit coords then updateCommand' status
                        else undragIfNeeded status
            where
                coords = cellFromCoord x y
                updateCommand' :: MouseStatus -> Maybe Command
                updateCommand' Clicked = dragIfNeeded coords com
                updateCommand' Released = dropIfNeeded coords com
                undragIfNeeded Released = Nothing
                undragIfNeeded Clicked = com

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
dragIfNeeded :: Coords -> Maybe Command -> Maybe Command
dragIfNeeded coords Nothing = Just $ DragAndDrop (Just coords) Nothing
dragIfNeeded _ c = c

dropIfNeeded :: Coords -> Maybe Command -> Maybe Command
dropIfNeeded coords (Just (DragAndDrop (Just p) Nothing))
    = Just $ DragAndDrop (Just p) (Just coords)
dropIfNeeded _ c = c

-- Given a x, y index, check it can be a position on the grid.
legit :: (Num a, Ord a) => (a, a) -> Bool
legit (x, y) = x < 8 && x >= 0 && y < 8 && y >= 0

-- Given a global mouse input expressed in gloss viewport,
-- convert it to a 0,0 coord system on the grid.
cellFromCoord :: Double -> Double -> Coords
cellFromCoord x y = (floor (x / 32.0), abs $ floor (y / 32.0))
