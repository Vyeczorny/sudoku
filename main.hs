import           Sudoku
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 0 then
    print "[ERROR] Filepath to file with board required"
  else do
    content <- readFile $ args !! 0
    print $ loadBoardFromFile content
