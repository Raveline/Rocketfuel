{-# LANGUAGE PackageImports #-}
import Rocketfuel.Grid
import Data.Char
import Control.Monad (unless)
import Rocketfuel.Display
import Rocketfuel.Input
import Rocketfuel.DisplayTypes
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT

main :: IO()
main = do let width = 800
              height = 600
              mouseState = Click (0.0, 0.0) Nothing
          -- Initialize GLUT, Elerea and GlossState
          (_,_) <- GLUT.getArgsAndInitialize
          (mouse, mouseSink) <- external mouseState
          glossState <- initState
          -- Get a window and loop
          withWindow width height "Rocketfuel" $ \window -> do
            -- Prepare the FRP network
            network <- start $ do
                gameContext <- transfer buildContext updateContext
                return $ displayContext window (width, height) glossState gameContext
            fix $ \loop -> do
                readMouse mouseState win mouseSink
                join network
                threadDelay 20000
                quit <- keyIsPressed window Key'Escape
                unless quit loop

updateContext :: Click -> GameContext -> GameContext
updateContext = undefined

-- Utility function to get a window
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
