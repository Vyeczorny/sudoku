import           Sudoku
import           System.Environment
import Data.List.Split

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 0 then
    print "[ERROR] Filepath to file with board required"
  else do
    content <- readFile $ args !! 0
    putStrLn "-----"
    mapM_ print $ chunksOf 9 $ generateAllPossibilities $ loadBoardFromFile content