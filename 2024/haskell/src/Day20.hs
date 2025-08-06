{-# LANGUAGE OverloadedRecordDot #-}

module Day20 (day20a, day20b) where

import Data.Hashable (Hashable)
import qualified Data.Map as M
import qualified Data.HashPSQ as H
import Data.Maybe (isNothing, fromJust)
import Algorithm (dijkstra)
import Linear (V2(V2))

data Maze = Maze {
        path :: [Point],
        start :: Point,
        end :: Point
    } deriving (Show)

type Point = V2 Int
type Cost = Int

{------------------------------{ 1st part }------------------------------}

day20a :: String -> String
day20a = show . part1 . parseLines . lines

parseLines :: [String] -> Maze
parseLines ls = Maze ps s e
  where
    ps = [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == '.' || c == 'E' || c == 'S']
    s = head $ [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == 'S']
    e = head $ [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == 'E']

part1 :: Maze -> Int
part1 maze = length . concatMap (filter (>= 100)) $ pathsLength
  where
    pathInOrder = reverse . fromJust . myDijkstra maze.start (getNeighbours' maze.path) (isGoal' maze.end) $ maze.end
    pathWithCost = zip pathInOrder [0..]
    pathMap = M.fromList pathWithCost
    pathsLength = map (computeShortcuts pathMap) pathInOrder

computeShortcuts :: M.Map Point Int -> Point -> [Int]
computeShortcuts pmap p = cuts
  where
    cuts = [dist | d <- dirs,
                   let p' = p + d * 2,
                   p' `M.member` pmap,
                   let dist = (pmap M.! p') - (pmap M.! p) - 2,
                   dist > 0]

dirs :: [Point]
dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

getNeighbours' :: [Point] -> Point -> [(Point, Cost)]
getNeighbours' ps p = [(p', 1) | d <- dirs, let p' = p + d,
                                                p' `elem` ps]

isGoal' :: Point -> Point -> Bool
isGoal' = (==)

myDijkstra :: (Num c, Ord c, Hashable v, Ord v) => v -> (v -> [(v, c)]) -> (v -> Bool) -> v -> Maybe [v]
myDijkstra s getNeighbours isGoal e
  | isNothing mapPaths = Nothing 
  | otherwise = Just (backtrace e)
    where
      mapPaths = dijkstra queue dist prev getNeighbours isGoal
      path' = fromJust mapPaths
      queue = H.singleton s 0 0
      dist = M.singleton s 0
      prev = M.empty 
      backtrace = go
        where
          go current
            | current `M.notMember` path' = [current]
            | otherwise = current : go (path' M.! current)

{------------------------------{ 2nd part }------------------------------}

day20b :: String -> String
day20b =  show . part2 . parseLines . lines

part2 :: Maze -> Int
part2 maze = length . concatMap (filter (>= 100)) $ pathsLength
  where
    pathInOrder = reverse . fromJust . myDijkstra maze.start (getNeighbours' maze.path) (isGoal' maze.end) $ maze.end
    pathWithCost = zip pathInOrder [0..]
    pathMap = M.fromList pathWithCost
    pathsLength = map (computeShortcuts' pathMap) pathInOrder

computeShortcuts' :: M.Map Point Int -> Point -> [Int]
computeShortcuts' pmap p = [dist | d <- dirs', 
                                   let p' = p + d,
                                   p' `M.member` pmap,
                                   let dist = (pmap M.! p') - (pmap M.! p) - manhattanDist p' p,
                                   dist > 0]

dirs' :: [Point]
dirs' = [V2 y x | y <- [-20..20], x <- [-20..20],
                  let dist = abs y + abs x,
                  dist >= 2 && dist <= 20]

manhattanDist :: Point -> Point -> Int
manhattanDist p1 p2 = sum . fmap abs $ p1 - p2
