{-# LANGUAGE DeriveGeneric #-}

module Exercises (histogram,renderMaze,markGuess,Check(..),Marking) where

-- Imports --
import Data.Array ( (!), (//), array )
import Data.List (delete)

-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n xs
    | n <= 0 = error "n must be greater than 0"
    | otherwise = map count [0..numRanges-1]
    where
        numRanges = (maximum xs `div` n) + 1
        count i = length $ filter (\x -> x `div` n == i) xs

-- Exercise A2
renderMaze :: [((Int,Int),(Int,Int))] -> [String]
renderMaze [] = []
renderMaze maze = map (map (maze'' !)) cells
  where
    (maxX, maxY) = foldr (\((x1,y1),(x2,y2)) (mx,my) -> (max mx (max x1 x2), max my (max y1 y2))) (0,0) maze

    cells = [[(x, y) | x <- [0..maxX]] | y <- [0..maxY]]

    maze' = array ((0, 0), (maxX, maxY)) [((x, y), ' ') | x <- [0..maxX], y <- [0..maxY]]
    maze'' = maze' // concatMap walls maze

    walls ((x1,y1),(x2,y2))
      | x1 == x2 && y1 <= y2 = [((x1,y), '#') | y <- [y1..y2]]
      | y1 == y2 && x1 <= x2 = [((x,y1), '#') | x <- [x1..x2]]
      | x1 == x2 && y1 > y2 = [((x1,y), '#') | y <- [y2..y1]]
      | y1 == y2 && x1 > x2 = [((x,y1), '#') | x <- [x2..x1]]
      | otherwise = error "invalid wall"

-- Exercise A3
data Check = Green | Yellow | Grey deriving (Eq, Show, Read)
type Marking = [(Char, Check)]

markGuess :: String -> String -> Marking
markGuess secret guess
  | length secret /= length guess = error "strings of different length not allowed"
  | otherwise = markYellow secret $ markGreen secret guess

markGreen :: String -> String -> Marking
markGreen [] _ = []
markGreen (s:ss) (g:gs)
  | s == g = (g, Green) : markGreen ss gs
  | otherwise = (g, Grey) : markGreen ss gs

markYellow :: String -> Marking -> Marking
markYellow _ [] = []
markYellow ss ((g, ms):gs)
  | ms == Green = (g, ms) : markYellow ss gs
  | g `elem` ss = (g, Yellow) : markYellow (delete g ss) gs
  | otherwise = (g, Grey) : markYellow ss gs

-- removeChar :: String -> Char -> String
-- removeChar [] _ = []
-- removeChar (s:ss) c
--   | s == c = ss
--   | otherwise = s : removeChar ss c
 