module Rocketfuel.Types (
    Grid,
    Line,
    Position,
    Cell (..), 
    Effect(..),
    Swap(..),
    GameContext(..),
    Command (..)
) where

data Cell
    = Fuel
    | Repair
    | Trade
    | Shoot
    | Navigate
    deriving (Eq, Ord, Show, Enum, Bounded)

type Position = (Integer, Integer)
data Swap = Swap Position Position

data Effect = Effect { _type :: Cell,
                       _size :: Int }
    deriving(Show)

type Grid = [Line]
type Line = [Maybe Cell]

data GameContext = GameContext { grid :: Grid,
                                 command :: Maybe Command }

data Command = DragAndDrop (Maybe Position) (Maybe Position)
