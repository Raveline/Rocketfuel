{-# LANGUAGE PackageImports #-}
module Rocketfuel.Display (
    GameContext(..),
    buildContext,
    displayContext,
    loadResources
) where 

import Data.Maybe
import qualified Data.Map as M

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

resources :: M.Map Cell FilePath
resources = M.fromList [(Fuel, "img/fuel.png")
                       ,(Navigate, "img/navigate.png")
                       ,(Repair, "img/repair.png")
                       ,(Shoot, "img/shoot.png")
                       ,(Trade, "img/trade.png")]

type CellTextures = M.Map Cell Picture

cellToResources :: Cell -> CellTextures -> Picture
cellToResources = flip (M.!)

buildContext :: IO GameContext
buildContext = do g <- generateRandomGrid
                  return $ GameContext g Nothing

loadResources :: IO CellTextures
loadResources = do res <- mapM loadJuicyPNG (M.elems resources)
                   if any isNothing res
                    then error "Missing resources"
                    else return $ M.fromList $ zip (M.keys resources) (catMaybes res)

displayContext :: Window -> (Int, Int) -> State -> CellTextures -> Grid -> IO ()
displayContext window (w, h) st res g = do
    displayPicture (w,h) white st (viewPortScale ortho0_0vp) 
        $ uncurry translate (viewPortTranslate ortho0_0vp) (displayGrid g res)
    swapBuffers window


-- | Take a list of list, and index it so that we can use those
-- as index of line and columns.
--
-- >>> idxListOfList $ ["word", "work", "worm"]
-- [(0,[(0,'w'),(1,'o'),('r',2),(3,'d')]), (1,[(0,'w'),(1,'o'),(2,'r'),(3,'k')])]
-- 
idxListOfList :: [[a]] -> [(Integer, [(Integer, a)])]
idxListOfList = zip [0..] . map (zip [0..])

displayGrid :: Grid -> CellTextures -> Picture
displayGrid grid res
    = Pictures . catMaybes . concatMap (pictureLine res) . idxListOfList $ grid

pictureLine :: CellTextures -> (Integer, [(Integer, Maybe Cell)]) -> [Maybe Picture]
pictureLine res (y, xx) = map (uncurry (pictureCell res y)) xx

pictureCell :: CellTextures -> Integer -> Integer -> Maybe Cell -> Maybe Picture
pictureCell res y x (Just c) = Just $ translate (fromIntegral x * 32.0)
                                                (fromIntegral y * (-32.0))
                                                (cellToResources c res)
pictureCell _ _ _ Nothing = Nothing
