{-# LANGUAGE PackageImports #-}
import Control.Monad (unless, join, when)
import Control.Monad.Fix
import Control.Concurrent (threadDelay)

import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Gloss.Rendering
import FRP.Elerea.Simple

import Rocketfuel.Display
import Rocketfuel.Input


main :: IO()
main = do let width = 800
              height = 600
              mouseState = Click (0.0, 0.0) Released
          -- Initialize GLUT, Elerea and GlossState
          (_,_) <- GLUT.getArgsAndInitialize
          (mouse, mouseSink) <- external mouseState
          glossState <- initState
          resources <- loadResources
          baseContext <- buildContext
          -- Get a window and loop
          withWindow width height "Rocketfuel" $ \window -> do
            -- Prepare the FRP network
            network <- start $ do
                gameContext <- transfer baseContext updateContext mouse
                return $ displayContext window (width, height) glossState resources <$> grid `fmap` gameContext
            fix $ \loop -> do
                readMouse window mouseSink
                join network
                threadDelay 20000
                quit <- keyIsPressed window Key'Escape
                unless quit loop


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
