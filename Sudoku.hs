module Sudoku (
  loadBoardFromFile,
  generateAllPossibilities,
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
solve board moves = solveAux board board moves 0
  where solveAux [] board _ _ = board
        solveAux (0 : _) board (moves : _) index = tryApplyMoves board (index, moves)
        solveAux (_ : nextValues) board (_ : nextMoves) index = solveAux nextValues board nextMoves (index + 1)

tryApplyMoves :: Board -> (Int, [Int]) -> Board
tryApplyMoves _ (_, []) = []
tryApplyMoves board (index, (move : nextMoves)) = 
  if solvedBoard /= [] then solvedBoard
  else tryApplyMoves board (index, nextMoves)
    where newBoard = setField board index move
          solvedBoard = solve newBoard (generateAllPossibilities newBoard)

generateAllPossibilities :: Board -> MovesBoard
generateAllPossibilities board = 
  map (\n -> 
    if board !! n == 0 then ((allNums \\ rows !! rowIndex n) \\ cols !! colIndex n ) \\ boxes !! boxIndex n
    else [board !! n]) allFieldIndexes
  where 
    rows = numsForRows board
    cols = numsForCols board
    boxes = numsForBoxes board


-- getNonSolvedField :: Board -> MovesBoard -> (Int, [Int])
-- getNonSolvedField board movesBoard = getNonSolvedFieldAux board movesBoard 0 (-1, [1..10])
--   where
--     getNonSolvedFieldAux [] _ _ (bestIndex, bestMoves) = (bestIndex, bestMoves)
--     getNonSolvedFieldAux (0 : nextValues) (moves : nextMoves) index (bestIndex, bestMoves) =
--       if movesLength == 1 then (index, moves)
--       else if movesLength < bestMovesLength && movesLength > 0 then getNonSolvedFieldAux nextValues nextMoves (index + 1) (index, moves)
--       else getNonSolvedFieldAux nextValues nextMoves (index + 1) (bestIndex, bestMoves)
--         where movesLength = length moves
--               bestMovesLength = length bestMoves
--     getNonSolvedFieldAux (_ : nextValues) (_ : nextMoves) index (bestIndex, bestMoves) = getNonSolvedFieldAux nextValues nextMoves (index + 1) (bestIndex, bestMoves)


-- private

loadBoardFromFile :: String -> Board
loadBoardFromFile content = map convertStringToInt $ words content

setField :: Board -> Int -> Int -> Board
setField (_ : nextValues) 0 newValue = (newValue : nextValues)
setField (value : nextValues) index newValue = (value : setField nextValues (index - 1) newValue)

convertStringToInt :: String -> Int
convertStringToInt str = if str == "." then 0 else read str :: Int

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