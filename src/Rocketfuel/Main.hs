{-# LANGUAGE PackageImports #-}
import Rocketfuel.Grid
import Data.Char
import Control.Monad (when)
import Rocketfuel.Display
import Rocketfuel.DisplayTypes
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT

main :: IO()
main = do let width = 800
              height = 600
          (_,_) <- GLUT.getArgsAndInitialize
          gameContext <- buildContext
          glossState <- initState
          withWindow width height "Rocketfuel" $ \window -> do
            loop (width, height) window glossState gameContext


withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
                m <- GLFW.createWindow width height title Nothing Nothing
                case m of
                    (Just win) -> do
                        GLFW.makeContextCurrent m
                        f win
                        GLFW.setErrorCallback $ Just simpleErrorCallback
                        GLFW.destroyWindow win
                    Nothing -> return ()
                GLFW.terminate
    where
        simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
