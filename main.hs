import Sudoku
import System.Environment
import System.Exit
import Data.Char
import UI.HSCurses.Curses as HSCurses
import UI.HSCurses.CursesHelper as HSHelpers
import Data.List.Split

main :: IO ()
main = do
  args <- getArgs
  if null args then
    print "[ERROR] Filepath to file with board required"
  else do
    file <- readFile $ head args
    let board = loadBoardFromFile file
    mapM_ print $ chunksOf 9 $ fields $ solveBoard board (generateAllMoves board)

type CursorPosition = (Int, Int)

grey :: Color
grey = Color 100

drawBoard :: Board -> IO ()
drawBoard board = do
  drawBoardAux (fields board) 0
  refresh
    where drawBoardAux [value] index = drawField index value
          drawBoardAux (value : nextValues) index = do
            drawField index value
            drawBoardAux nextValues (index + 1)

positionForIndex :: Int -> (Int, Int)
positionForIndex index = (row, col)
  where 
        row = (index `div` 9) * 2
        col = (index `mod` 9) * 4

drawField :: Int -> Int -> IO ()
drawField index value = do
  let (row, col) = positionForIndex index
  attrSet attr0 (Pair 2)
  mvWAddStr stdScr row       col "-----"
  mvWAddStr stdScr (row + 1) col "|   |"
  mvWAddStr stdScr (row + 2) col "-----"
  attrSet attr0 (Pair 1)
  mvWAddStr stdScr (row + 1) (col + 2) (if value == 0 then " " else show value)
 
drawCursor :: (Int, Int) -> IO ()
drawCursor (row, col) = do
  attrSet attr0 (Pair 1)
  mvWAddStr stdScr (row * 2)     (col * 4)     "+---+"
  mvWAddStr stdScr (row * 2 + 2) (col * 4)     "+---+"
  mvWAddStr stdScr (row * 2 + 1) (col * 4)     "|"
  mvWAddStr stdScr (row * 2 + 1) (col * 4 + 4) "|"
  refresh

drawInfoBar :: String -> IO ()
drawInfoBar info = do
  (srcRows, srcCols) <- scrSize
  if info == "" then mvWAddStr stdScr (srcRows - 1) 0 $ replicate (srcCols - 10) ' '  
  else mvWAddStr stdScr (srcRows - 1) 0 info

drawMenu :: IO ()
drawMenu = do
  attrSet attr0 (Pair 3)
  mvWAddStr stdScr 0 40 "         Menu         "
  mvWAddStr stdScr 1 40 " Arrows "
  mvWAddStr stdScr 2 40 " 0-9    "
  mvWAddStr stdScr 3 40 " h      "
  mvWAddStr stdScr 4 40 " q      "

  attrSet attr0 (Pair 1)
  mvWAddStr stdScr 1 48 " Show cursor"
  mvWAddStr stdScr 2 48 " Insert value"
  mvWAddStr stdScr 3 48 " Show hint"
  mvWAddStr stdScr 4 48 " Quit"

-- main :: IO ()
-- main = do
--   window <- initScr
--   args <- getArgs
--   file <- readFile $ head args

--   echo False
--   keypad window True
--   cursSet CursorInvisible

--   startColor
--   initColor grey (128, 128, 128)
--   initPair (Pair 1) white black
--   initPair (Pair 2) grey black
--   initPair (Pair 3) black white

--   runGame (loadBoardFromFile file) (0, 0)

runGame :: Board -> CursorPosition -> IO ()
runGame board (row, col) = do
  drawBoard board
  drawMenu
  Main.drawCursor (row, col)
  c <- getCh
  if c == KeyLeft then 
    runGame board (row, (col - 1) `mod` 9)  
  else if c == KeyRight then 
    runGame board (row, (col + 1) `mod` 9)
  else if c == KeyUp then
    runGame board ((row - 1) `mod` 9, col)
  else if c == KeyDown then
    runGame board ((row + 1) `mod` 9, col)
  else let KeyChar char = c in
    if char == 'q' then
      delWin stdScr >> endWin >> exitWith ExitSuccess
    else if char `elem` ['1','2','3','4','5','6','7','8','9'] then
      runGame (setField board (9 * row + col) (digitToInt char)) (row, col)
    else if char == ' ' then
      runGame (setField board (9 * row + col) 0) (row, col)
  else do
    drawInfoBar $ "Unrecognized key: " ++ show c
    runGame board (row, col)




  -- c <- getCh
  -- if c == KeyChar 'q' then delWin stdScr >> endWin >> exitWith ExitSuccess
  -- else if c == KeyLeft then do
  --     -- put (loadBoardFromFile file, (0, 0))
  --     print ""
  -- else do
  --       mvWAddStr stdScr 7 200 $ "You pressed: " ++ show c
  --     --   clrToEol
  --     --   mvWAddStr window 7 10 $ "You pressed: " ++ show c
  --     --   refresh







