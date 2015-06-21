import Rocketfuel.Grid (Grid, Cell(..), generateRandomGrid,
                        Swap(..), applySwap, afterMove)
import Data.Char
import Rocketfuel.Display

main :: IO()
main = do g <- generateRandomGrid
          r <- loadResources
          gc <- return $ GameContext r g
          loop gc

gridTest :: IO ()
gridTest = do g <- generateRandomGrid
              displayGrid g
              loop' g

loop' :: Grid -> IO ()
loop' g = do input <- getLine
             if input == "quit" || input == "exit"
                then return ()
                else processSwap input g

processSwap :: String -> Grid -> IO ()
processSwap s g = case sToSwap s of
                    Just s' -> do g' <- return $ applySwap s' g
                                  (g'', effs) <- afterMove g'
                                  mapM_ (putStrLn . show) effs
                                  displayGrid g''
                                  loop' g''
                    Nothing -> do putStrLn "Cannot parse !"
                                  displayGrid g
                                  loop' g

sToSwap :: String -> Maybe Swap
sToSwap s = do one' <- toPos one
               second' <- toPos second
               Just $ Swap one' second'
    where (one, second) = splitAt 4 s
          toPos (n:s:n2:xxx)
            | isDigit n && isDigit n2 = Just (digitToInt n, digitToInt n2)
            | otherwise = Nothing
          toPos _ = Nothing
        
displayGrid :: Grid -> IO ()
displayGrid g = do putStrLn $ " " ++ (concat . map show $ [0..7])
                   mapM_ displayLine $ zip [0..] g

displayLine :: (Integer, [Maybe Cell]) -> IO ()
displayLine (n, l) = let asString = map (maybe ' ' cellToChar) l in
                     putStrLn $ (show n) ++ asString

cellToChar :: Cell -> Char
cellToChar Fuel = 'F'
cellToChar Repair = 'R'
cellToChar Trade = 'T'
cellToChar Shoot = 'S'
cellToChar Navigate = 'N'
