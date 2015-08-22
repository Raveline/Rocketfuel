{-# LANGUAGE PackageImports #-}
module Rocketfuel.Display (
    GameContext(..),
    buildContext,
    displayContext
) where 

import Data.Maybe

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy

import Rocketfuel.Grid
import Rocketfuel.Types
import Rocketfuel.DisplayTypes

ortho0_0vp :: ViewPort
ortho0_0vp = ViewPort (-384, 284) 0 1

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

buildContext :: IO GameContext
buildContext = do g <- generateRandomGrid
                  rs <- loadResources
                  return $ GameContext rs g Nothing

loadResources :: IO [Picture]
loadResources = do res <- mapM loadJuicyPNG resources
                   if any isNothing res
                    then error "Missing resources"
                    else return $ catMaybes res

displayContext :: Window -> (Int, Int) -> State -> GameContext -> IO ()
displayContext window (w, h) st gc = do
    displayPicture (w,h) white st (viewPortScale ortho0_0vp) 
        $ uncurry translate (viewPortTranslate ortho0_0vp) (displayGrid gc)
    swapBuffers window


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
