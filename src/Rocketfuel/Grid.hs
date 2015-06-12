module Rocketfuel.Grid (
    Cell (..), 
    Grid,
    Line
) where

import Data.List
import Control.Monad.Writer

data Effect = Effect { _type :: Cell,
                       _size :: Int }

data Cell
    = Fuel
    | Repair
    | Trade
    | Shoot
    | Navigate
    | Empty
    deriving (Eq, Show, Enum)

type Grid = [[Cell]]
type Line = [Cell]
type Idx = Int
type Length = Int

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

rotateGrid :: Grid -> Grid
-- | Basic utility to get a clockwise 90° rotation on the grid,
-- useful to handle columns as if they were lines.
-- 
-- >>> rotateGrid [[Fuel,Repair,Trade],[Fuel,Shoot,Navigate],[Fuel,Trade,Empty]]
-- [[Fuel,Fuel,Fuel],[Repair,Shoot,Trade],[Trade,Navigate,Empty]]
rotateGrid = transpose

unrotateGrid :: Grid -> Grid
-- | Basic utility to get a 90° counterclockwise 90° rotation on the grid.
-- >>> rotateGrid [[Fuel,Fuel,Fuel],[Repair,Shoot,Trade],[Trade,Navigate,Empty]]
-- [[Fuel,Repair,Trade],[Fuel,Shoot,Navigate],[Fuel,Trade,Empty]]
unrotateGrid = transpose . transpose . transpose
