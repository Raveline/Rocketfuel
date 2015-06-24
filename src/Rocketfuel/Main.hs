import Rocketfuel.Grid
import Data.Char
import Rocketfuel.Display
import Rocketfuel.DisplayTypes

main :: IO()
main = do g <- generateRandomGrid
          r <- loadResources
          gc <- return $ GameContext r g Nothing
          loop gc
