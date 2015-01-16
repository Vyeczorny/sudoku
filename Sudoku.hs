module Sudoku (
  loadBoardFromFile,
  generateAllPossibilities
) where

import Data.List.Split
import Data.List

type Value = Int
type Board = [Value]
type PossibilitiesBoard = [[Value]]

allNums :: [Int]
allNums = [1..9]

allIndexes :: [Int]
allIndexes = [0..8]

allFieldIndexes :: [Int]
allFieldIndexes = [0..80]

-- public

generateAllPossibilities :: Board -> PossibilitiesBoard
generateAllPossibilities board = 
  map (\n -> 
    if board !! n == 0 then ((allNums \\ rows !! rowIndex n) \\ cols !! colIndex n ) \\ boxes !! boxIndex n
    else [board !! n]) allFieldIndexes
  where 
    rows = numsForRows board
    cols = numsForCols board
    boxes = numsForBoxes board

-- private

loadBoardFromFile :: String -> Board
loadBoardFromFile content = map convertStringToInt $ words content

convertStringToInt :: String -> Int
convertStringToInt str = if str == "_" then 0 else read str :: Int

numsForRows :: Board -> [[Int]]
numsForRows board = chunksOf 9 board

numsForCols :: Board -> [[Int]]
numsForCols board = map numsForCol allIndexes
  where numsForCol col = [board !! (9 * i + col) | i <- allIndexes]

numsForBoxes :: Board -> [[Int]]
numsForBoxes board = map numsForBox allIndexes
  where numsForBox n = [board !! (27 * (n `div` 3) + 3 * (n `mod` 3) + i) | i <- [0,1,2,9,10,11,18,19,20]]

boxIndex :: Int -> Int
boxIndex n = (n `div` 27) * 3 + (n `mod` 9 `div` 3)

rowIndex :: Int -> Int
rowIndex n = n `div` 9

colIndex :: Int -> Int
colIndex n = n `mod` 9