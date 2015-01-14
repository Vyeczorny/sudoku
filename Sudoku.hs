module Sudoku (
  loadBoardFromFile,
  generateAllPossibilities
) where

import Data.List

type Row = [Int]
type Board = [Row]
type BoardPossibilities = [[[Int]]]

allNums :: [Int]
allNums = [1,2,3,4,5,6,7,8,9]

allIndexes :: [Int]
allIndexes = [0,1,2,3,4,5,6,7,8]

loadBoardFromFile :: String -> Board
loadBoardFromFile content = map convertStringsToInts $ map words $ lines content

generateAllPossibilities :: Board -> BoardPossibilities
generateAllPossibilities board = 
  map (\ row -> map (generateAllPossibilitiesForField board row) allIndexes) allIndexes
  where rows = numsInRows board
        columns = numsInCols board
        boxes = numsInBoxes board
        removeAllExistingNums row col = ((allNums \\ rows !! row) \\ columns !! col) \\ boxes !! (row `div` 3) !! (col `div` 3)
        generateAllPossibilitiesForField b row col = 
          if b !! row !! col /= 0 then [b !! row !! col]
          else removeAllExistingNums row col


-- private

convertStringsToInts :: [String] -> [Int]
convertStringsToInts stringsList = map (\x -> if x == "_" then 0 else read x) stringsList :: [Int]

numsInRows :: Board -> [[Int]]
numsInRows [] = []
numsInRows (firstRow : anotherRows) = (delete 0 . nub) firstRow : numsInRows anotherRows

numsInCols :: Board -> [[Int]]
numsInCols ([] : _) = []
numsInCols board = (delete 0 . nub . concatMap (\x -> [head x])) board : (numsInCols . map tail) board

numsInBoxes :: Board -> [[[Int]]]
numsInBoxes [] = []
numsInBoxes board = (numsInBoxesAux . take 3) board : numsInBoxes (drop 3 board)

numsInBoxesAux :: [[Int]] -> [[Int]]
numsInBoxesAux [[], [], []] = []
numsInBoxesAux rows = (filter (/= 0) . concatMap (take 3)) rows : (numsInBoxesAux . map (drop 3)) rows