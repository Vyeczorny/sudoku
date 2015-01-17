module Sudoku (
  loadBoardFromFile,
  generateAllMoves,
  solve
) where

import Data.List.Split
import Data.List

type Value = Int
type Board = [Value]
type MovesBoard = [[Value]]

allNums :: [Int]
allNums = [1..9]

allIndexes :: [Int]
allIndexes = [0..8]

allFieldIndexes :: [Int]
allFieldIndexes = [0..80]

-- public

solve :: Board -> MovesBoard -> Board
solve board movesBoard = 
  if index == -1 then
    if isBoardSolved board then board
    else []
  else tryApplyMoves board (index, moves)
    where (index, moves) = getNonSolvedField board movesBoard

tryApplyMoves :: Board -> (Int, [Int]) -> Board
tryApplyMoves _ (_, []) = []
tryApplyMoves board (index, move : nextMoves) = 
  if solvedBoard /= [] then solvedBoard
  else tryApplyMoves board (index, nextMoves)
    where newBoard = setField board index move
          solvedBoard = solve newBoard (generateAllMoves newBoard)

isBoardSolved :: Board -> Bool
isBoardSolved [] = True
isBoardSolved (0 : _) = False
isBoardSolved (_ : nextValues) = isBoardSolved nextValues

generateAllMoves :: Board -> MovesBoard
generateAllMoves board = 
  map (\n -> 
    if board !! n == 0 then ((allNums \\ rows !! rowIndex n) \\ cols !! colIndex n ) \\ boxes !! boxIndex n
    else [board !! n]) allFieldIndexes
  where 
    rows = numsForRows board
    cols = numsForCols board
    boxes = numsForBoxes board

getNonSolvedField :: Board -> MovesBoard -> (Int, [Int])
getNonSolvedField board movesBoard = getNonSolvedFieldAux board movesBoard 0 (-1, [1..10])
  where
    getNonSolvedFieldAux [] _ _ (bestIndex, bestMoves) = (bestIndex, bestMoves)
    getNonSolvedFieldAux (0 : nextValues) (moves : nextMoves) index (bestIndex, bestMoves)
      | movesLength == 1 = (index, moves)
      | movesLength < bestMovesLength && movesLength > 0 = getNonSolvedFieldAux nextValues nextMoves (index + 1) (index, moves)
      | otherwise = getNonSolvedFieldAux nextValues nextMoves (index + 1) (bestIndex, bestMoves)
        where movesLength = length moves
              bestMovesLength = length bestMoves
    getNonSolvedFieldAux (_ : nextValues) (_ : nextMoves) index (bestIndex, bestMoves) = getNonSolvedFieldAux nextValues nextMoves (index + 1) (bestIndex, bestMoves)

-- private

loadBoardFromFile :: String -> Board
loadBoardFromFile content = map convertStringToInt $ words content

setField :: Board -> Int -> Int -> Board
setField (_ : nextValues) 0 newValue = newValue : nextValues
setField (value : nextValues) index newValue = value : setField nextValues (index - 1) newValue

convertStringToInt :: String -> Int
convertStringToInt str = if str == "." then 0 else read str :: Int

numsForRows :: Board -> [[Int]]
numsForRows = chunksOf 9 

numsForCols :: Board -> [[Int]]
numsForCols board = map (\n -> getEvery9 $ drop n board) allIndexes
  where getEvery9 [] = []
        getEvery9 _board = head _board : getEvery9 (drop 9 _board)

numsForBoxes :: Board -> [[Int]]
numsForBoxes board = map (\n -> getValuesOnIndexes board 0 (fieldsInBox !! n)) allIndexes
  where getValuesOnIndexes _ _ [] = []
        getValuesOnIndexes (value : nextValues) fieldIndex (index : nextIndexes) =
          if fieldIndex == index then value : getValuesOnIndexes nextValues (fieldIndex + 1) nextIndexes
          else getValuesOnIndexes nextValues (fieldIndex + 1) (index : nextIndexes)

fieldsInBox :: [[Int]]
fieldsInBox = [
                [0,1,2,9,10,11,18,19,20],
                [3,4,5,12,13,14,21,22,23],
                [6,7,8,15,16,17,24,25,26],
                [27,28,29,36,37,38,45,46,47],
                [30,31,32,39,40,41,48,49,50],
                [33,34,35,42,43,44,51,52,53],
                [54,55,56,63,64,65,72,73,74],
                [57,58,59,66,67,68,75,76,77],
                [60,61,62,69,70,71,78,79,80]
              ]

boxIndex :: Int -> Int
boxIndex n = (n `div` 27) * 3 + (n `mod` 9 `div` 3)

rowIndex :: Int -> Int
rowIndex n = n `div` 9

colIndex :: Int -> Int
colIndex n = n `mod` 9