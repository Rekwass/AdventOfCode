{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day01 (day01a, day01b) where

import Data.List.Split (splitOn)
import Data.List (foldl')
import qualified Data.Set as S
import Lib.Numbers (manhattan)

{------------------------------{ 1st part }------------------------------}

data Turn = L Int | R Int deriving Show
data Direction = N | E | S | W deriving (Show, Eq, Ord)

type CoordD = (Int, Int, Direction)
type Coord = (Int, Int)

day01a :: String -> String
day01a = show . part1 . parseLine

parseLine :: String -> [Turn]
parseLine = map toTurn . splitOn ", "
  where
    toTurn ('R':xs) = R $ read xs
    toTurn ('L':xs) = L  $ read xs

part1 :: [Turn] -> Int
part1 = manhattan . toCoord . foldl' step (0, 0, N)

step :: CoordD -> Turn -> CoordD
step (y, x, d) t = move (turn d t)
  where
    turn N (L _) = W
    turn W (L _) = S
    turn S (L _) = E
    turn E (L _) = N
    turn N (R _) = E
    turn E (R _) = S
    turn S (R _) = W
    turn W (R _) = N

    move d' = let (dy, dx) = delta d' in (y + dy * n, x + dx * n, d')

    n = case t of L k -> k; R k -> k

delta :: Direction -> Coord
delta N = (-1, 0)
delta E = (0 , 1)
delta S = (1 , 0)
delta W = (0 , -1)

toCoord :: CoordD -> Coord
toCoord (y, x, _) = (y, x)

{------------------------------{ 2nd part }------------------------------}

day01b :: String -> String
day01b = show . part2 . parseLine

part2 :: [Turn] -> Int
part2 xs = manhattan $ findFirstDuped $ expandPath $ scanl step (0, 0, N) xs

expandPath :: [CoordD] -> [Coord]
expandPath xs = concat $ zipWith expand xs (tail xs)
  where
    expand (y1,x1,_) (y2,x2,_)
      | x1 == x2 = [(y,x1) | y <- range y1 y2]
      | y1 == y2 = [(y1,x) | x <- range x1 x2]
      | otherwise = []
    range a b | a < b     = [a+1..b]
              | otherwise = [a-1, a-2..b]

findFirstDuped :: [Coord] -> Coord
findFirstDuped = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = x
      | otherwise         = go (S.insert x seen) xs
