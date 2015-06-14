{-# LANGUAGE GeneralizedNewtypeDeriving #-}  

module Rocketfuel.Grid (
    Cell (..), 
    Effect(..),
    Grid,
    Line,
    generateRandomGrid
) where

import Data.List
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
    | Empty
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
generateRandomLine = sequence (replicate 8 generateRandomCell)

generateRandomGrid :: (MonadRandom r) => r Grid
generateRandomGrid = sequence (replicate 8 generateRandomLine)

type Grid = [Line]
type Line = [Cell]

emptyAndLogIfAboveThree :: Line -> Writer [Effect] Line
emptyAndLogIfAboveThree line
    | length line >= 3 = let effect = [Effect (head line) (length line)] in
                         tell effect
                         >> mapM (\_ -> return Empty) line
    | otherwise = return line

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
