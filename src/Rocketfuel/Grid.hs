module Rocketfuel.Grid (
    Cell (..), 
    Effect(..),
    Swap(..),
    Position,
    Grid,
    Line,
    generateRandomGrid,
    afterMove,
    applySwap
) where

import Data.Array
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Control.Monad.Writer
import Control.Monad.Random


data Effect = Effect { _type :: Cell,
                       _size :: Int }
    deriving(Show)

type Position = (Integer, Integer)
data Swap = Swap Position Position

data Cell
    = Fuel
    | Repair
    | Trade
    | Shoot
    | Navigate
    deriving (Eq, Show, Enum, Bounded)

-- These lines allow us to run random and randomR on an enum
getRandomR' :: (MonadRandom m, Enum a, Bounded a) => (a,a) -> m a
getRandomR' (a, b) = uniform [a..b]

generateRandomCell :: (MonadRandom r) => r Cell
generateRandomCell = getRandomR'(Fuel, Navigate)

generateRandomLine :: (MonadRandom r) => r Line
generateRandomLine = replicateM 8 (liftM Just generateRandomCell)

generateRandomGrid :: (MonadRandom r) => r Grid
generateRandomGrid = replicateM 8 generateRandomLine

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
-- >>> rotateGrid [[Just Fuel,Just Repair,Just Trade],[Just Fuel,Just Shoot,Just Navigate],[Just Fuel,Just Trade,Just Trade]]
-- [[Just Fuel,Just Fuel,Just Fuel],[Just Repair,Just Shoot,Just Trade],[Just Trade,Just Navigate,Just Trade]]
rotateGrid :: Grid -> Grid
rotateGrid = transpose

-- | Basic utility to get a 90° counterclockwise 90° rotation on the grid.
-- >>> rotateGrid [[Just Fuel,Just Fuel,Just Fuel],[Just Repair,Just Shoot,Just Trade],[Just Trade,Just Navigate,Nothing]]
-- [[Just Fuel,Just Repair,Just Trade],[Just Fuel,Just Shoot,Just Navigate],[Just Fuel,Just Trade,Nothing]]
unrotateGrid :: Grid -> Grid
unrotateGrid = transpose . transpose . transpose

-- | The main matching function that will look for matches in lines and columns,
-- and log each matches.
emptyGrid :: Grid -> Writer [Effect] Grid
emptyGrid g = emptyLines g >>= emptyColumns
    where emptyLines :: Grid -> Writer [Effect] Grid
          emptyLines = mapM emptyRepeted
          emptyColumns :: Grid -> Writer [Effect] Grid
          emptyColumns g' = do let rotated = rotateGrid g'
                               columnsEmptied <- emptyLines rotated
                               return $ unrotateGrid columnsEmptied

-- |In order to simulate gravity, we proceed like this :
-- 1°) We prepend to the grid a generated line
-- (this is done in another function, to keep this one pure)
-- The generated line CANNOT contain empty cells.
-- 2°) We rotate the grid, so as to be able to map on its column.
-- 3°) For each column, apply this simple rule :
--     - If the current element is not empty...
--     - and the next element is empty...
--     - ... then the current element will take its place
-- 4°) Remove the first line, containing the generated, random cells
-- This will make all cells go down one notch if they should.
-- Then, new possible match should be checked again before running
-- this till there is no change.
--
-- >>> gravity [[Just Fuel,Just Fuel,Just Repair,Just Shoot],[Nothing,Nothing,Nothing,Nothing],[Just Fuel,Nothing,Just Trade,Nothing]]
-- [[Nothing,Nothing,Nothing,Nothing],[Just Fuel,Nothing,Just Repair,Nothing],[Just Fuel,Just Fuel,Just Trade,Just Shoot]]
gravity :: Grid -> Grid
gravity = tail . unrotateGrid . map gravityColumn . rotateGrid
    where 
          gravityColumn :: Line -> Line
          gravityColumn = until filledAtBottom falling
          filledAtBottom :: Line -> Bool
          filledAtBottom = anyNull . break isNothing
          falling :: Line -> Line
          falling (x:Nothing:ys) = Nothing:falling (x:ys)
          falling (x:y:ys) = x:falling (y:ys)
          falling ys = ys
          anyNull (f, s) = null f || null s

-- |Used before calling gravity. 
prependGrid :: (MonadRandom r) => Grid -> r Grid
prependGrid ls = do newLine <- generateRandomLine 
                    return $ newLine:ls

containsEmptyCells :: Grid -> Bool
-- |Check if a grill contains any empty cell.
-- >>> containsEmptyCells [[Just Fuel, Just Fuel], [Nothing, Just Shoot]]
-- True
-- >>> containsEmptyCells [[Just Fuel, Just Fuel], [Just Shoot, Just Shoot]]
-- False
containsEmptyCells = any isNothing . concat

-- | Call gravity till there is no empty cell left in the grid
gravityLoop :: (MonadRandom r) => Grid -> r Grid
gravityLoop g
    | containsEmptyCells g  = liftM gravity (prependGrid g) >>= gravityLoop
    | otherwise =  return g

-- | After a given move, we must :
-- - Check for matchs.
-- - If matches were made, apply gravity and loop.
-- - If no matches were made, return the grid and the effects.
afterMove :: (MonadRandom r) => Grid -> r (Grid, [Effect])
afterMove = afterMove' []
    where
        afterMove' :: (MonadRandom r) => [Effect] -> Grid -> r (Grid, [Effect])
        afterMove' eff g = case runWriter (emptyGrid g) of
                            (g', []) -> return (g', eff)
                            (g', e) -> do afterGravity <- gravityLoop g'
                                          afterMove' (eff ++ e) afterGravity

-- | Applying a swap means :
-- 1°) Transform the grid into a single array to allow for easier
-- manipulation
-- 2°) Swap the position in the single array
-- 3°) Transform the array back into a list
--
applySwap :: Swap -> Grid -> Grid
applySwap (Swap p1 p2) g = rebuild . elems $ toArray // [(oneIdx, twoValue), (twoIdx, oneValue)]
    where
        toArray :: Array Integer Cell
        toArray = listArray (0, 63) . concatMap catMaybes $ g
        idx2Dto1D :: Position -> Integer
        idx2Dto1D (x, y) = y*8 + x
        oneIdx = idx2Dto1D p1
        twoIdx = idx2Dto1D p2
        oneValue = toArray ! oneIdx
        twoValue = toArray ! twoIdx
        rebuild = map (map Just) . chunksOf 8
