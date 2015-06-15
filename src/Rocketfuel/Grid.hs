{-# LANGUAGE GeneralizedNewtypeDeriving #-}  

module Rocketfuel.Grid (
    Cell (..), 
    Effect(..),
    Grid,
    Line,
    generateRandomGrid
) where

import Data.List
import Data.Maybe
import Control.Monad.Writer
import Control.Monad.Random

data Effect = Effect { _type :: Cell,
                       _size :: Int }

data Cell
    = Fuel
    | Repair
    | Trade
    | Shoot
    | Navigate
    deriving (Eq, Show, Enum, Bounded)

-- These lines allow us to run random and randomR on an enum
boundedEnumRandomR :: (Bounded a, Enum a, RandomGen g) =>
                      (a, a) -> g -> (a, g)
boundedEnumRandomR (x, y) g = case randomR (fromEnum x, fromEnum y) g of
                                (r, g') -> (toEnum r, g')
getRandomR' :: (MonadRandom m, Enum a, Bounded a) => (a,a) -> m a
getRandomR' (a, b) = uniform [a..b]

generateRandomCell :: (MonadRandom r) => r Cell
generateRandomCell = getRandomR'(Fuel, Navigate)

generateRandomLine :: (MonadRandom r) => r Line
generateRandomLine = sequence (replicate 8 (generateRandomCell >>= return . Just))

generateRandomGrid :: (MonadRandom r) => r Grid
generateRandomGrid = sequence (replicate 8 generateRandomLine)

type Grid = [Line]
type Line = [Maybe Cell]

emptyAndLogIfAboveThree :: Line -> Writer [Effect] Line
emptyAndLogIfAboveThree line
    | length line' >= 3 = let effect = [Effect (head line') (length line')] in
                         tell effect
                         >> mapM (\_ -> return Nothing) line
    | otherwise = return line
    where line' = catMaybes line

emptyRepeted :: Line -> Writer [Effect] Line
emptyRepeted l = do groups <- return . group $ l 
                    result <- mapM emptyAndLogIfAboveThree groups
                    return . concat $ result

-- | Basic utility to get a clockwise 90° rotation on the grid,
-- useful to handle columns as if they were lines.
-- 
-- >>> rotateGrid [[Fuel,Repair,Trade],[Fuel,Shoot,Navigate],[Fuel,Trade,Empty]]
-- [[Fuel,Fuel,Fuel],[Repair,Shoot,Trade],[Trade,Navigate,Empty]]
rotateGrid :: Grid -> Grid
rotateGrid = transpose

-- | Basic utility to get a 90° counterclockwise 90° rotation on the grid.
-- >>> rotateGrid [[Fuel,Fuel,Fuel],[Repair,Shoot,Trade],[Trade,Navigate,Empty]]
-- [[Fuel,Repair,Trade],[Fuel,Shoot,Navigate],[Fuel,Trade,Empty]]
unrotateGrid :: Grid -> Grid
unrotateGrid = transpose . transpose . transpose

emptyGrid :: Grid -> Writer [Effect] Grid
emptyGrid g = emptyLines g >>= emptyColumns
    where emptyLines :: Grid -> Writer [Effect] Grid
          emptyLines = mapM emptyRepeted
          emptyColumns :: Grid -> Writer [Effect] Grid
          emptyColumns g = do let rotated = rotateGrid g
                              columnsEmptied <- emptyLines rotated
                              return $ unrotateGrid columnsEmptied

-- In order to simulate gravity, we proceed like this :
-- 1°) We prepend to the grid a generated line
-- (this is done in another function, to keep this one pure)
-- The generated line CANNOT contain empty cells.
-- 2°) We rotate the grid, so as to be able to map on its column.
-- 3°) For each column, apply this simple rule :
--     - If the current element is not empty...
--     - and the next element is empty...
--     - ... then the current element will take its place
-- This will make all cells go down one notch if they should.
-- Then, new possible match should be checked again before running
-- this till there is no change.
gravity :: Grid -> Grid
gravity = unrotateGrid . map gravityColumn . rotateGrid
    where 
          gravityColumn :: Line -> Line
          gravityColumn = until (filledAtBottom) falling
          filledAtBottom :: Line -> Bool
          filledAtBottom = not . any (isNothing) . snd . break (isNothing)
          falling :: Line -> Line
          falling (x:Nothing:ys) = Nothing:falling (x:ys)
          falling (x:y:ys) = x:falling (y:ys)
          falling ys = ys
