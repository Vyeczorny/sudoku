module Sudoku (
  Board, fields, constFields,
  MovesBoard,
  loadBoardFromFile,
  setField,
  solveBoard,
  getHint,
  isBoardCorrect,
  isBoardSolved,
  findIncorrectFieldIfAny
) where

import Data.List.Split
import Data.List
import System.IO.Unsafe
import System.Random

type Value = Int
type Index = Int
type BoardData = [Value]
data Board = Board
  { fields       :: BoardData
  , constFields :: [Index]
  } deriving Show
data IncorrectField = Row Int | Column Int | Box Int deriving Show
type MovesBoard = [[Value]]

allNums :: [Int]
allNums = [1..9]

allIndexes :: [Int]
allIndexes = [0..8]

allFieldIndexes :: [Int]
allFieldIndexes = [0..80]

-- public

solveBoard :: Board -> Maybe Board
solveBoard board = case solve (fields board) (generateAllMoves (fields board)) of
  [] -> Nothing
  boardData -> Just Board { fields = boardData, constFields = getAllNonZeroFields boardData }

loadBoardFromFile :: String -> Board
loadBoardFromFile file = Board { fields = boardData, constFields = nonZeroFields }
  where boardData = map convertStringToInt $ words file
        nonZeroFields = getAllNonZeroFields boardData

setField :: Board -> Int -> Int -> Maybe Board
setField board index newValue = 
  if index `elem` constFields board then Nothing
  else Just Board { fields = setFieldOnBoardData (fields board) index newValue, constFields = constFields board }

getHint :: Board -> Maybe (Int, Int)
getHint board = case isBoardSolved board || not (isBoardCorrect board) of
  True -> Nothing
  False -> case getAllZeroFields $ fields board of 
    [] -> Nothing
    zeroFields -> case solveBoard board of
      Nothing -> Nothing
      Just solvedBoard -> Just (fieldIndex, (fields solvedBoard) !! fieldIndex)
        where randNumber = unsafePerformIO $ randomRIO (0, length zeroFields - 1)
              fieldIndex = zeroFields !! randNumber

isBoardCorrect :: Board -> Bool
isBoardCorrect board = isBoardCorrectAux f 0
  where f = fields board
        isBoardCorrectAux [] _ = True
        isBoardCorrectAux (value : nextValues) index =
          if value /= 0 && (not $ isFieldCorrect f value index) then False
          else isBoardCorrectAux nextValues (index + 1)

findIncorrectFieldIfAny :: Board -> Maybe IncorrectField
findIncorrectFieldIfAny board = findIncorrectFieldIfAnyAux f 0
  where f = fields board
        findIncorrectFieldIfAnyAux [] _ = Nothing
        findIncorrectFieldIfAnyAux (value : nextValues) index =
          if value /= 0 then
            if not $ isFieldCorrectInRow f value index then Just $ Row $ rowIndex index
            else if not $ isFieldCorrectInCol f value index then Just $ Column $ colIndex index
            else if not $ isFieldCorrectInBox f value index then Just $ Box $ boxIndex index
            else findIncorrectFieldIfAnyAux nextValues (index + 1)
          else findIncorrectFieldIfAnyAux nextValues (index + 1)

isBoardSolved :: Board -> Bool
isBoardSolved board = isBoardCorrect board && (isBoardDataSolved $ fields board)

-- private 

solve :: BoardData -> MovesBoard -> BoardData
solve board movesBoard = 
  if index == -1 then
    if isBoardDataSolved board then board
    else []
  else tryApplyMoves board (index, moves)
    where (index, moves) = getNonSolvedField board movesBoard

isBoardDataSolved :: BoardData -> Bool
isBoardDataSolved [] = True
isBoardDataSolved (0 : _) = False
isBoardDataSolved (_ : nextValues) = isBoardDataSolved nextValues

isFieldCorrect :: BoardData -> Int -> Int -> Bool
isFieldCorrect board value index =
  ([value, value] \\ (numsForRow board $ rowIndex index)) == [value] &&
  ([value, value] \\ (numsForCol board $ colIndex index)) == [value] &&
  ([value, value] \\ (numsForBox board $ boxIndex index)) == [value]

isFieldCorrectInRow :: BoardData -> Int -> Int -> Bool
isFieldCorrectInRow board value index = ([value, value] \\ (numsForRow board $ rowIndex index)) == [value]

isFieldCorrectInCol :: BoardData -> Int -> Int -> Bool
isFieldCorrectInCol board value index = ([value, value] \\ (numsForCol board $ colIndex index)) == [value]

isFieldCorrectInBox :: BoardData -> Int -> Int -> Bool
isFieldCorrectInBox board value index = ([value, value] \\ (numsForBox board $ boxIndex index)) == [value]


tryApplyMoves :: BoardData -> (Int, [Int]) -> BoardData
tryApplyMoves _ (_, []) = []
tryApplyMoves board (index, move : nextMoves) = 
  if solvedBoard /= [] then solvedBoard
  else tryApplyMoves board (index, nextMoves)
    where newBoard = setFieldOnBoardData board index move
          solvedBoard = solve newBoard (generateAllMoves newBoard)

generateAllMoves :: BoardData -> MovesBoard
generateAllMoves board = 
  map (\n -> 
    if board !! n == 0 then ((allNums \\ rows !! rowIndex n) \\ cols !! colIndex n ) \\ boxes !! boxIndex n
    else [board !! n]) allFieldIndexes
  where 
    rows = numsForRows board
    cols = numsForCols board
    boxes = numsForBoxes board

getNonSolvedField :: BoardData -> MovesBoard -> (Int, [Int])
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

getAllNonZeroFields :: BoardData -> [Index]
getAllNonZeroFields board = getAllNonZeroFieldsAux board 0
  where getAllNonZeroFieldsAux [] _ = []
        getAllNonZeroFieldsAux (value : nextValues) index = 
          if value /= 0 then index : getAllNonZeroFieldsAux nextValues (index + 1)
          else getAllNonZeroFieldsAux nextValues (index + 1)

getAllZeroFields :: BoardData -> [Index]
getAllZeroFields board = getAllZeroFieldsAux board 0
  where getAllZeroFieldsAux [] _ = []
        getAllZeroFieldsAux (value : nextValues) index = 
          if value == 0 then index : getAllZeroFieldsAux nextValues (index + 1)
          else getAllZeroFieldsAux nextValues (index + 1)

setFieldOnBoardData :: BoardData -> Int -> Int -> BoardData
setFieldOnBoardData (_ : nextValues) 0 newValue = newValue : nextValues
setFieldOnBoardData (value : nextValues) index newValue = value : setFieldOnBoardData nextValues (index - 1) newValue

convertStringToInt :: String -> Int
convertStringToInt str = if str == "." then 0 else read str :: Int

numsForRows :: BoardData -> [[Int]]
numsForRows = chunksOf 9 

numsForCols :: BoardData -> [[Int]]
numsForCols board = map (\n -> getEvery9 $ drop n board) allIndexes
  where getEvery9 [] = []
        getEvery9 _board = head _board : getEvery9 (drop 9 _board)

numsForBoxes :: BoardData -> [[Int]]
numsForBoxes board = map (\n -> getValuesOnIndexes board 0 (fieldsInBox !! n)) allIndexes
  where getValuesOnIndexes _ _ [] = []
        getValuesOnIndexes (value : nextValues) fieldIndex (index : nextIndexes) =
          if fieldIndex == index then value : getValuesOnIndexes nextValues (fieldIndex + 1) nextIndexes
          else getValuesOnIndexes nextValues (fieldIndex + 1) (index : nextIndexes)

numsForCol :: BoardData -> Int -> [Value]
numsForCol board col = numsForCols board !! col

numsForRow :: BoardData -> Int -> [Value]
numsForRow board row = numsForRows board !! row

numsForBox :: BoardData -> Int -> [Value]
numsForBox board box = numsForBoxes board !! box

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