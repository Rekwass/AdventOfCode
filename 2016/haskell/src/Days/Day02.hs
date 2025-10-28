{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Days.Day02 (day02a, day02b) where

import Data.List (foldl')
import Lib.Numbers (manhattan)

{------------------------------{ 1st part }------------------------------}

type Coord = (Int, Int)

day02a :: String -> String
day02a = show . part1 . lines

keyPad :: [[Int]]
keyPad = [[1,2,3],
          [4,5,6],
          [7,8,9]]

part1 :: [String] -> Int
part1 = read . concatMap (show . convertLine)

convertLine :: String -> Int
convertLine = coordToNum . foldl' step (1,1)

coordToNum :: Coord -> Int
coordToNum (y, x) = keyPad !! y !! x

step :: Coord -> Char -> Coord
step p@(y, x) c
  | isOOB p'  = p
  | otherwise = p'
    where
      p' = let (dy, dx) = delta c in (y + dy, x + dx)

delta :: Char -> Coord
delta 'U' = (-1, 0)
delta 'R' = (0 , 1)
delta 'D' = (1 , 0)
delta 'L' = (0 , -1)

isOOB :: Coord -> Bool
isOOB (y, x) = x < 0 || y < 0 || x > 2 || y > 2 

{------------------------------{ 2nd part }------------------------------}

day02b :: String -> String
day02b = show . part2 . lines

keyPad' :: [[Char]]
keyPad' = [['1'],
      ['2', '3', '4'],
 ['5', '6', '7', '8', '9'],
      ['A', 'B', 'C'],
           ['D']]

part2 :: [String] -> String
part2 = map convertLine'

convertLine' :: String -> Char
convertLine' = coordToNum' . convertCoord . foldl' step' (0,-2)

coordToNum' :: Coord -> Char
coordToNum' (y, x) = keyPad' !! y !! x

step' :: Coord -> Char -> Coord
step' p@(y, x) c
  | isOOB' p' = p
  | otherwise = p'
    where
      p' = let (dy, dx) = delta c in (y + dy, x + dx)

isOOB' :: Coord -> Bool
isOOB' = (>2) . manhattan

convertCoord :: Coord -> Coord
convertCoord (y, x)
  | i == 2    = (y', x)
  | i == 1    = (y', x + 1)
  | otherwise = (y', x + 2)
  where
    i = abs y
    y' = y + 2
