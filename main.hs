import Sudoku
import System.Environment
import System.Exit
import Data.Char
import UI.HSCurses.Curses as HSCurses
import UI.HSCurses.CursesHelper as HSHelpers

-- initializations

type CursorPosition = (Int, Int)
data BoardState = BoardState { boardData :: Board
                             , cursorPosition :: CursorPosition
                             , infoBarState :: String 
                             }

grey :: Color
grey = Color 100

lightGrey :: Color
lightGrey = Color 101

drawBoard :: Board -> IO ()
drawBoard boardState = do
  drawBoardAux (fields boardState) 0
  refresh
    where drawBoardAux [value] index = drawField index value (index `elem` constFields boardState)
          drawBoardAux (value : nextValues) index = do
            drawField index value (index `elem` constFields boardState)
            drawBoardAux nextValues (index + 1)

positionForIndex :: Int -> (Int, Int)
positionForIndex index = (row, col)
  where 
        row = (index `div` 9) * 2
        col = (index `mod` 9) * 4

-- manipulating cursor

moveCursorLeft :: CursorPosition -> CursorPosition
moveCursorLeft (row, col) = (row, (col - 1) `mod` 9)

moveCursorRight :: CursorPosition -> CursorPosition
moveCursorRight (row, col) = (row, (col + 1) `mod` 9)

moveCursorUp :: CursorPosition -> CursorPosition
moveCursorUp (row, col) = ((row - 1) `mod` 9, col)

moveCursorDown :: CursorPosition -> CursorPosition
moveCursorDown (row, col) = ((row + 1) `mod` 9, col)

cursorIndex :: CursorPosition -> Int
cursorIndex (row, col) = 9 * row + col

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
  mvWAddStr stdScr 5 40 " c      "
  mvWAddStr stdScr 6 40 " q      "

  attrSet attr0 (Pair 1)
  mvWAddStr stdScr 1 48 " Show cursor"
  mvWAddStr stdScr 2 48 " Insert value"
  mvWAddStr stdScr 3 48 " Show hint"
  mvWAddStr stdScr 4 48 " Solve board"
  mvWAddStr stdScr 5 48 " Check solution"
  mvWAddStr stdScr 6 48 " Quit"

-- main

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "Filepath to sudoku board missing"
    (fileName : _) -> do
      file <- readFile fileName
      window <- initScr

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

      runGame BoardState { boardData = loadBoardFromFile file, cursorPosition = (0,0), infoBarState = "Welcome! New sudoku is waiting for you" }

runGame :: BoardState -> IO ()
runGame state = do
  drawBoard $ boardData state
  drawInfoBar $ infoBarState state
  drawMenu
  Main.drawCursor $ cursorPosition state

  let boardState = state { infoBarState = "" }

  c <- getCh
  case c of
    KeyLeft      -> runGame $ boardState { cursorPosition = moveCursorLeft $ cursorPosition boardState }
    KeyRight     -> runGame $ boardState { cursorPosition = moveCursorRight $ cursorPosition boardState }
    KeyUp        -> runGame $ boardState { cursorPosition = moveCursorUp $ cursorPosition boardState }
    KeyDown      -> runGame $ boardState { cursorPosition = moveCursorDown $ cursorPosition boardState }
    KeyChar 'q'  -> delWin stdScr >> endWin >> exitSuccess
    KeyChar 'h'  -> case getHint $ boardData boardState of
      Nothing ->
        runGame $ boardState { infoBarState = "Application cannot gives you any hints" }
      Just (pos, value) -> case setField (boardData boardState) pos value of
        Nothing ->
          runGame $ boardState { infoBarState = "Cannot change value at index: " ++ show pos }
        Just newBoardState -> 
          runGame $ boardState { boardData = newBoardState }
    KeyChar 's'  -> case solveBoard (boardData boardState) of 
      Nothing ->
        runGame $ boardState { infoBarState = "This board hasn't got a solution. Consider removing some digits" }
      Just 
        newBoardState -> runGame $ boardState { boardData = newBoardState }
    KeyChar 'c' -> if isBoardSolved (boardData boardState) then
                      runGame $ boardState { infoBarState = "Board solved" }
                   else case findIncorrectFieldIfAny $ boardData boardState of
                      Nothing -> runGame $ boardState { infoBarState = "Board is not solved" }
                      Just index -> runGame $ boardState { infoBarState = "Board is not solved, conflicting fields in " ++ show index }
    KeyChar char -> 
      if char `elem` "123456789 " then
        case setField (boardData boardState) (cursorIndex $ cursorPosition boardState) (if char == ' ' then 0 else digitToInt char) of 
          Nothing->
            runGame $ boardState { infoBarState = "Cannot change value at point: " ++ show (cursorPosition boardState) }
          Just newBoardState -> 
            runGame $ boardState { boardData = newBoardState }
      else
        runGame $ boardState { infoBarState = "Unrecognized key: " ++ show c }