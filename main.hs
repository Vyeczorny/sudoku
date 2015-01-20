import Sudoku
import System.Environment
import System.Exit
import Data.Char
import UI.HSCurses.Curses as HSCurses
import UI.HSCurses.CursesHelper as HSHelpers
-- import Data.List.Split

-- main :: IO ()
-- main = do
--   args <- getArgs
--   if null args then
--     print "[ERROR] Filepath to file with board required"
--   else do
--     file <- readFile $ head args
--     let sudokuBoard = loadBoardFromFile file
--     mapM_ print $ chunksOf 9 $ fields $ solveBoard sudokuBoard

type CursorPosition = (Int, Int)

-- initializations

grey :: Color
grey = Color 100

lightGrey :: Color
lightGrey = Color 101

drawBoard :: Board -> IO ()
drawBoard sudokuBoard = do
  drawBoardAux (fields sudokuBoard) 0
  refresh
    where drawBoardAux [value] index = drawField index value (index `elem` constFields sudokuBoard)
          drawBoardAux (value : nextValues) index = do
            drawField index value (index `elem` constFields sudokuBoard)
            drawBoardAux nextValues (index + 1)

positionForIndex :: Int -> (Int, Int)
positionForIndex index = (row, col)
  where 
        row = (index `div` 9) * 2
        col = (index `mod` 9) * 4

-- drawing functions

drawField :: Int -> Int -> Bool -> IO ()
drawField index value isConstField = do
  let (row, col) = positionForIndex index
  attrSet attr0 (Pair 2)
  mvWAddStr stdScr row       col "-----"
  mvWAddStr stdScr (row + 1) col "|   |"
  mvWAddStr stdScr (row + 2) col "-----"
  if isConstField then attrSet attr0 (Pair 4)
  else attrSet attr0 (Pair 1)
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
  mvWAddStr stdScr (srcRows - 1) 0 $ replicate (srcCols - 10) ' '  
  mvWAddStr stdScr (srcRows - 1) 0 info

drawMenu :: IO ()
drawMenu = do
  attrSet attr0 (Pair 3)
  mvWAddStr stdScr 0 40 "         Menu         "
  mvWAddStr stdScr 1 40 " Arrows "
  mvWAddStr stdScr 2 40 " 0-9    "
  mvWAddStr stdScr 3 40 " h      "
  mvWAddStr stdScr 4 40 " s      "
  mvWAddStr stdScr 5 40 " q      "

  attrSet attr0 (Pair 1)
  mvWAddStr stdScr 1 48 " Show cursor"
  mvWAddStr stdScr 2 48 " Insert value"
  mvWAddStr stdScr 3 48 " Show hint"
  mvWAddStr stdScr 4 48 " Solve board"
  mvWAddStr stdScr 5 48 " Quit"

-- main

main :: IO ()
main = do
  window <- initScr
  args <- getArgs
  file <- readFile $ head args

  echo False
  keypad window True
  cursSet CursorInvisible

  startColor
  initColor grey (128, 128, 128)
  initColor lightGrey (256, 256, 256)
  initPair (Pair 1) white black
  initPair (Pair 2) grey black
  initPair (Pair 3) black white
  initPair (Pair 4) lightGrey black

  runGame (loadBoardFromFile file) (0, 0)

runGame :: Board -> CursorPosition -> IO ()
runGame sudokuBoard (row, col) = do
  drawBoard sudokuBoard
  drawMenu
  Main.drawCursor (row, col)
  c <- getCh
  if c == KeyLeft then 
    runGame sudokuBoard (row, (col - 1) `mod` 9)  
  else if c == KeyRight then 
    runGame sudokuBoard (row, (col + 1) `mod` 9)
  else if c == KeyUp then
    runGame sudokuBoard ((row - 1) `mod` 9, col)
  else if c == KeyDown then
    runGame sudokuBoard ((row + 1) `mod` 9, col)
  else let KeyChar char = c in
    if char == 'q' then
      delWin stdScr >> endWin >> exitSuccess
    else if char == 'h' then
      drawInfoBar "HINT"
    else if char == 's' then
      case solveBoard sudokuBoard of 
        Nothing -> do
          drawInfoBar $ "This board hasn't got a solution. Consider removing some digits"
          runGame sudokuBoard (row, col)
        Just newSudokuBoard -> runGame newSudokuBoard (row, col)
    else if char `elem` "123456789 " then do
      case setField sudokuBoard (9 * row + col) (if char == ' ' then 0 else (digitToInt char)) of 
        Nothing -> do
          drawInfoBar $ "Cannot change value at point: " ++ show row ++ "," ++ show col
          runGame sudokuBoard (row, col)
        Just newSudokuBoard -> runGame newSudokuBoard (row, col)
  else do
    drawInfoBar $ "Unrecognized key: " ++ show c
    runGame sudokuBoard (row, col)




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







