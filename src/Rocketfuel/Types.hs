module Rocketfuel.Types (
    Grid,
    GameGrid,
    Line,
    Position,
    Moves,
    Cell (..), 
    Effect(..),
    Swap(..),
    GameContext(..),
    Command (..),
    World (..)
) where

import Data.Natural

-- The components of a Grid
data Cell
    = Fuel
    | Repair
    | Trade
    | Shoot
    | Navigate
    deriving (Eq, Ord, Show, Enum, Bounded)

-- A basic 2D position
type Position = (Integer, Integer)
-- A type to encode swapping between two positions
data Swap = Swap Position Position

-- Effect of a grid combination
data Effect = Effect { _type :: Cell,
                       _size :: Int }
    deriving(Show)

type Grid a = [[a]]

type Moves = [Maybe Natural]
type GameGrid = Grid (Maybe Cell)

-- A Line is a list of potential cells
type Line = [Maybe Cell]

-- The main container for a game, made of a grid
-- and the general world context
data GameContext = GameContext { grid :: GameGrid,
                                 world :: World }

-- This type should represent the situation of the rocket,
-- the distance from its arrival point, the current score,
-- the potential enemies, etc.
data World = World { _distance :: Int,
                     _cash :: Int }

-- Result of a player input.
data Command = DragAndDrop (Maybe Position) (Maybe Position)
