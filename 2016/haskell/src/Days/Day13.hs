module Days.Day13 (day13a, day13b) where

import Data.Bits (popCountDefault)
import Lib.Algorithm (bfsDist, bfsNbExplored)
import Linear.V2 (V2 (V2))

{------------------------------{ 1st part }------------------------------}

type Point = V2 Int

day13a :: String -> String
day13a = show . part1 . parseLines . lines

parseLines :: [String] -> Int
parseLines [x] = read x
parseLines _   = error "impossible"

part1 :: Int -> Int
part1 n = bfsDist (V2 1 1) (getNeighbours n) isGoal

getNeighbours :: Int -> Point -> [Point]
getNeighbours n (V2 y x) = filter (not . isWall n) ps
  where
    ps = [ V2 (y + dy) (x + dx) | (dy, dx) <- dirs, y + dy >= 0 && x + dx >= 0]

dirs :: [(Int, Int)]
dirs = [(0, 1), (-1, 0), (0, -1), (1, 0)]

isGoal :: Point -> Bool
isGoal (V2 39 31) = True -- Real
isGoal _      = False

isWall :: Int -> V2 Int -> Bool
isWall n (V2 y x) = odd $ popCountDefault res
  where
    res = x * x + 3 * x + 2 * x * y + y + y * y + n

{------------------------------{ 2nd part }------------------------------}

day13b :: String -> String
day13b = show . part2 . parseLines . lines

part2 :: Int -> Int
part2 n = bfsNbExplored (V2 1 1) (getNeighbours n) 50
