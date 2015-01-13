module Sudoku (
  loadBoardFromFile
) where

type Field = Integer
type Column = [Field]
type Board = [Column]
type BoardPossibilities = [[[Int]]]

loadBoardFromFile :: String -> Board
loadBoardFromFile content = map convertStringsToInts $ map words $ lines content

-- private

convertStringsToInts :: [String] -> [Integer]
convertStringsToInts stringsList = map (\x -> if x == "_" then 0 else read x) stringsList :: [Integer]