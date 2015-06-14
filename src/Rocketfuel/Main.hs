import Rocketfuel.Grid (Grid, Cell(..), generateRandomGrid)

main :: IO ()
main = generateRandomGrid >>= displayGrid
        
displayGrid :: Grid -> IO ()
displayGrid = mapM_ displayLine

displayLine :: [Cell] -> IO ()
displayLine l = let asString = map cellToChar l in 
              putStrLn asString

cellToChar :: Cell -> Char
cellToChar Fuel = 'F'
cellToChar Repair = 'R'
cellToChar Trade = 'T'
cellToChar Shoot = 'S'
cellToChar Navigate = 'N'
cellToChar Empty = ' '
