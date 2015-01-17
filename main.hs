 import           Data.List.Split
 import           Sudoku
 import           System.Environment

 main :: IO ()
 main = do
   args <- getArgs
   if null args then
     print "[ERROR] Filepath to file with board required"
   else do
     content <- readFile $ head args
     mapM_ print $ chunksOf 9 $ solve (loadBoardFromFile content) (generateAllMoves $ loadBoardFromFile content)