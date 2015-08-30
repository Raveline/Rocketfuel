module Rocketfuel.Grid (
    generateRandomGrid,
    afterMove,
    applySwap,
    getFallingTiles,
    hasMoves
) where

import Data.Array
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Natural
import Control.Monad.Writer
import Control.Monad.Random

import Rocketfuel.Types


-- These lines allow us to run random and randomR on an enum
getRandomR' :: (MonadRandom m, Enum a, Bounded a) => (a,a) -> m a
getRandomR' (a, b) = uniform [a..b]

generateRandomCell :: (MonadRandom r) => r Cell
generateRandomCell = getRandomR'(Fuel, Navigate)

generateRandomLine :: (MonadRandom r) => r Line
generateRandomLine = replicateM 8 (liftM Just generateRandomCell)

generateRandomGrid :: (MonadRandom r) => r GameGrid
generateRandomGrid = replicateM 8 generateRandomLine

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
rotateGrid :: Grid a -> Grid a
rotateGrid = transpose

-- | Basic utility to get a 90° counterclockwise 90° rotation on the grid.
-- >>> rotateGrid [[Just Fuel,Just Fuel,Just Fuel],[Just Repair,Just Shoot,Just Trade],[Just Trade,Just Navigate,Nothing]]
-- [[Just Fuel,Just Repair,Just Trade],[Just Fuel,Just Shoot,Just Navigate],[Just Fuel,Just Trade,Nothing]]
unrotateGrid :: Grid a -> Grid a
unrotateGrid = transpose . transpose . transpose

-- | The main matching function that will look for matches in lines and columns,
-- and log each matches.
emptyGrid :: GameGrid -> Writer [Effect] GameGrid
emptyGrid g = emptyLines g >>= emptyColumns
    where emptyLines :: GameGrid -> Writer [Effect] GameGrid
          emptyLines = mapM emptyRepeted
          emptyColumns :: GameGrid -> Writer [Effect] GameGrid
          emptyColumns g' = do let rotated = rotateGrid g'
                               columnsEmptied <- emptyLines rotated
                               return $ unrotateGrid columnsEmptied

-- |Find the index of the LAST element of a list
-- matching a predicate.
-- >>> findLastIndex (==2) [2,2,2,2]
-- Just (Natural 3)
-- >>> findLastIndex (==2) []
-- Nothing
-- >>> findLastIndex (==2) [1,3,5]
-- Nothing
-- >>> findLastIndex (=='z') "zoning zoning"
-- Just (Natural 7)
findLastIndex :: (a -> Bool) -> [a] -> Maybe Natural
findLastIndex p xs = fmap reverseIndex (findIndex p . reverse $ xs)
    where maxIndex = length xs - 1
          reverseIndex :: Int -> Natural
          reverseIndex = natural . fromIntegral . (-) maxIndex

getFallingTiles :: GameGrid -> Moves
getFallingTiles = map gravityColumn . rotateGrid
    where gravityColumn :: Line -> Maybe Natural
          gravityColumn = findLastIndex isNothing

-- >>> gravity [[Just Fuel,Just Fuel,Just Repair,Just Shoot],[Nothing,Nothing,Nothing,Nothing],[Just Fuel,Nothing,Just Trade,Nothing]]
-- [[Nothing,Nothing,Nothing,Nothing],[Just Fuel,Nothing,Just Repair,Nothing],[Just Fuel,Just Fuel,Just Trade,Just Shoot]]
gravity :: GameGrid -> GameGrid
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
prependGrid :: (MonadRandom r) => GameGrid -> r GameGrid
prependGrid ls = do newLine <- generateRandomLine 
                    return $ newLine:ls

containsEmptyCells :: GameGrid -> Bool
-- |Check if a grill contains any empty cell.
-- >>> containsEmptyCells [[Just Fuel, Just Fuel], [Nothing, Just Shoot]]
-- True
-- >>> containsEmptyCells [[Just Fuel, Just Fuel], [Just Shoot, Just Shoot]]
-- False
containsEmptyCells = any isNothing . concat

-- | Call gravity till there is no empty cell left in the grid
gravityLoop :: (MonadRandom r) => GameGrid -> r GameGrid
gravityLoop g
    | containsEmptyCells g  = liftM gravity (prependGrid g) >>= gravityLoop
    | otherwise =  return g

-- | After a given move, we must :
-- - Check for matchs.
-- - If matches were made, apply gravity and loop.
-- - If no matches were made, return the grid and the effects.
afterMove :: (MonadRandom r) => GameGrid -> r (GameGrid, [Effect])
afterMove = afterMove' []
    where 
        afterMove' :: (MonadRandom r) => [Effect] -> GameGrid -> r (GameGrid, [Effect])
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
applySwap :: Swap -> GameGrid -> GameGrid
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

-- |Given a list of moves, check if there is any current move.
-- This simply means checking that the list of Moves (one per column in the grid)
-- is only made out of Nothing.
--
-- >>> hasMoves [Nothing, Nothing, Nothing]
-- False
-- >>> hasMoves [Nothing, Nothing, Just 3]
-- True
hasMoves :: Moves -> Bool
hasMoves = any (isJust)
