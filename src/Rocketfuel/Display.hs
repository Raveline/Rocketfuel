module Rocketfuel.Display (
    loop,
    GameContext(..),
    loadResources
) where 

import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

import Rocketfuel.Grid

data GameContext = GameContext { _resources :: [Picture],
                                 grid :: Grid }

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
displayContext = InWindow "Rocketfuel" (0,0) (480,640)

loop :: GameContext -> IO ()
loop game = play displayContext black 60 game displayGrid eventHandling stepHandling

idxListOfList :: [[a]] -> [(Integer, [(Integer, a)])]
idxListOfList = zip [1..] . map (zip [1..])

displayGrid :: GameContext -> Picture
displayGrid (GameContext res _grid) 
    = Pictures . catMaybes . concatMap (pictureLine res) . idxListOfList $ _grid

pictureLine :: [Picture] -> (Integer, [(Integer, Maybe Cell)]) -> [Maybe Picture]
pictureLine res (y, xx) = map (uncurry (pictureCell res y)) xx

pictureCell :: [Picture] -> Integer -> Integer -> Maybe Cell -> Maybe Picture
pictureCell res y x (Just c) = Just $ translate (fromIntegral y * 16.0) 
                                                (fromIntegral x * 16.0)
                                                (cellToResources res c)
pictureCell _ _ _ Nothing = Nothing

eventHandling :: Event -> GameContext -> GameContext
eventHandling _ g = g

stepHandling :: Float -> GameContext -> GameContext
stepHandling _ g = g
