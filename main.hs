import           Sudoku
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 0 then
    print "[ERROR] Filepath to file with board required"
  else do
    content <- readFile $ args !! 0
    putStrLn "-----"
    mapM_ print $ solveBoard $ loadBoardFromFile content
    -- mapM_ print $ generateAllPossibilities $ loadBoardFromFile content
    -- print $ getNextMove (loadBoardFromFile content) (generateAllPossibilities $ loadBoardFromFile content) 1 1
    -- print (getMove content)
    -- mapM_ print $ applyMove (board content) (getMove content)

board content = loadBoardFromFile content
poss content = generateAllPossibilities $ board content
getMove content = getNextMove (board content) (poss content) 1 1