import Rocketfuel.Grid
import Data.Char
import Rocketfuel.Display
import Rocketfuel.DisplayTypes

main :: IO()
main = do let width = 640
              height = 480
          g <- generateRandomGrid
          r <- loadResources
          gc <- return $ GameContext r g Nothing
          withWindow width height "Rocketfuel" $ \loop gc
