module Sudoku (
  loadBoardFromFile
) where

import Data.List.Split

type Field = Int
type Board = [Field]

allNums :: [Int]
allNums = [1,2,3,4,5,6,7,8,9]

allIndexes :: [Int]
allIndexes = [0,1,2,3,4,5,6,7,8]

-- public

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