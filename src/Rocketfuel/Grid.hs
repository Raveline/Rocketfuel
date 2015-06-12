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
emptyRepeted l = return . group $ l 
                 >>= mapM emptyAndLogIfAboveThree
                 >>= concat
