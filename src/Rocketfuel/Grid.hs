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

type Grid = [Line]
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
          emptyColumns g = do rotated <- return $ rotateGrid g
                              columnsEmptied <- emptyLines rotated
                              return $ unrotateGrid $ columnsEmptied
