import Rocketfuel.Grid (Grid, Cell(..), generateRandomGrid,
                        Swap(..), applySwap, afterMove)
import Data.Char
import Rocketfuel.Display

main :: IO()
main = do g <- generateRandomGrid
          r <- loadResources
          gc <- return $ GameContext r g Nothing
          loop gc
