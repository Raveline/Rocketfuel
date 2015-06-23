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

type DragDropState = Maybe Position

data GameContext = GameContext { _resources :: [Picture],
                                 grid :: Grid,
                                 dragDropState :: DragDropState }

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

loop :: GameContext -> IO ()
loop game = play displayContext black 60 game displayGrid eventHandling stepHandling

idxListOfList :: [[a]] -> [(Integer, [(Integer, a)])]
idxListOfList = zip [0..] . map (zip [0..])

displayGrid :: GameContext -> Picture
displayGrid (GameContext res _grid _) 
    = translate (-320.0) (120.0) $ Pictures . catMaybes . concatMap (pictureLine res) . idxListOfList $ _grid

pictureLine :: [Picture] -> (Integer, [(Integer, Maybe Cell)]) -> [Maybe Picture]
pictureLine res (y, xx) = map (uncurry (pictureCell res y)) xx

pictureCell :: [Picture] -> Integer -> Integer -> Maybe Cell -> Maybe Picture
pictureCell res y x (Just c) = Just $ translate (fromIntegral y * 32.0) 
                                                (fromIntegral x * 32.0)
                                                (cellToResources res c)
pictureCell _ _ _ Nothing = Nothing

cellFromCoord :: Float -> Float -> (Integer, Integer)
cellFromCoord x y = (round (globalX / 32.0), 7 - round (globalY / 32.0))
    where globalX = x + 320
          globalY = y - 120

modifyIfNeeded :: Float -> Float -> GameContext -> GameContext
modifyIfNeeded x y g@(GameContext _ _ Nothing) 
    = if uncurry legit coords
            then traceShow coords $ g { dragDropState = Just coords }
            else g
    where coords = cellFromCoord x y
          legit x y = x < 8 && x >= 0 && y < 8 && y >= 0
modifyIfNeeded _ _ g = g

eventHandling :: Event -> GameContext -> GameContext
eventHandling (EventKey (MouseButton _) Down _ (x, y)) g = modifyIfNeeded x y g
eventHandling _ g = g

stepHandling :: Float -> GameContext -> GameContext
stepHandling _ g = g
