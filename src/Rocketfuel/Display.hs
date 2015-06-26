module Rocketfuel.Display (
    loop,
    GameContext(..),
    loadResources
) where 

import Data.Maybe

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

import Rocketfuel.Grid
import Rocketfuel.Input
import Rocketfuel.Types
import Rocketfuel.DisplayTypes


resources :: [String]
resources = ["img/fuel.png",
             "img/navigate.png",
             "img/repair.png",
             "img/shoot.png",
             "img/trade.png"]

-- This is a disgrace, but fortunately a temporary one. 
-- We'll have to consider using at least a map (Cell, Picture).
cellToResources :: [Picture] -> Cell -> Picture
cellToResources res Fuel = head res
cellToResources res Navigate = res !! 1
cellToResources res Repair = res !! 2
cellToResources res Shoot = res !! 3
cellToResources res Trade = res !! 4

loadResources :: IO [Picture]
loadResources = do res <- mapM loadJuicyPNG resources
                   if any isNothing res
                        then error "Missing resources"
                        else return $ catMaybes res

displayContext :: Display
displayContext = InWindow "Rocketfuel" (640, 480) (10, 10)

loop :: (Int, Int) -> GameContext -> IO ()
loop w game = do pollEvents
                 displayContext game
                 threadDelay 20000
                 k <- keyIsPressed window Key'Escape
                 if k
                    then return ()
                    else loop w game

-- | Take a list of list, and index it so that we can use those
-- as index of line and columns.
--
-- >>> idxListOfList $ ["word", "work", "worm"]
-- [(0,[(0,'w'),(1,'o'),('r',2),(3,'d')]), (1,[(0,'w'),(1,'o'),(2,'r'),(3,'k')])]
-- 
idxListOfList :: [[a]] -> [(Integer, [(Integer, a)])]
idxListOfList = zip [0..] . map (zip [0..])

displayGrid :: GameContext -> Picture
displayGrid (GameContext res _grid _) 
    = Pictures . catMaybes . concatMap (pictureLine res) . idxListOfList $ _grid

pictureLine :: [Picture] -> (Integer, [(Integer, Maybe Cell)]) -> [Maybe Picture]
pictureLine res (y, xx) = map (uncurry (pictureCell res y)) xx

pictureCell :: [Picture] -> Integer -> Integer -> Maybe Cell -> Maybe Picture
pictureCell res y x (Just c) = Just $ translate (fromIntegral x * 32.0)
                                                (fromIntegral y * (-32.0))
                                                (cellToResources res c)
pictureCell _ _ _ Nothing = Nothing

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

eventHandling :: Event -> GameContext -> GameContext
eventHandling (EventKey (MouseButton _) Down _ (x, y)) g = dragIfNeeded x y g
eventHandling (EventKey (MouseButton _) Up _ (x, y)) g = dropIfNeeded x y g
eventHandling _ g = g

stepHandling :: Float -> GameContext -> GameContext
stepHandling _ g = g
