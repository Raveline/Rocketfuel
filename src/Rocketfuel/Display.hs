{-# Language PackageImports #-}
module Rocketfuel.Display (
    withWindow
) where 

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad

withWindow :: Int -> Int -> String
              -> (GLFW.Window -> IO ()) -> IO ()
withWindow w h t f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow w h t Nothing Nothing
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
