{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day16 (day16a, day16b) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.HashPSQ as H
import Data.Hashable (Hashable)
import Linear.V2 (V2 (V2))
import Data.Maybe (fromJust, mapMaybe)
import Algorithm.Search (dijkstraAssoc)

data Maze = Maze {
        points :: S.Set Point,
        start :: Point,
        end :: Point
    } deriving (Show)

type Node = (Point, Direction)
type Point = V2 Int
type Direction = Point
type Cost = Int

data Graph v c =
  MkGraph
    { vertices   :: S.Set v
    , neighbours :: v -> [(v, c)]
    }

{------------------------------{ 1st part }------------------------------}

day16a :: String -> String
day16a = show . part1 . parseLines . lines

parseLines :: [String] -> Maze
parseLines ls =
    let
      ps = S.fromList [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == '.' || c == 'E' || c == 'S']
      s = head $ [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == 'S']
      e = head $ [V2 y x | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c == 'E']
    in Maze ps s e

part1 :: Maze -> Int
part1 (Maze ps s e) = fst . fromJust . dijkstraAssoc (getNeighbours' ps) (isGoal' e) $ (s, V2 0 1)

getNeighbours' :: S.Set Point -> Node -> [(Node, Cost)]
getNeighbours' ps (p, d)
    | (p + d) `S.member` ps = inFront : turns
    | otherwise = turns
    where
      inFront = ((p + d, d), 1)
      turns = [((p, turnLeft d), 1000), ((p, turnRight d), 1000)]

isGoal' :: Point -> Node -> Bool
isGoal' e c = e == fst c

turnLeft :: Direction -> Direction
turnLeft (V2 (-1) 0) = V2 0 (-1)
turnLeft (V2 0 1) = V2 (-1) 0
turnLeft (V2 1 0) = V2 0 1
turnLeft (V2 0 (-1)) = V2 1 0

turnRight :: Direction -> Direction
turnRight (V2 (-1) 0) = V2 0 1
turnRight (V2 0 1) = V2 1 0
turnRight (V2 1 0) = V2 0 (-1)
turnRight (V2 0 (-1)) = V2 (-1) 0

-- displayMaze :: Maze -> String
-- displayMaze (Maze ps s@(V2 ys _) e@(V2 _ xe)) = intercalate "\n" [line y | y <- [0..(ys + 1)]]
--     where
--       line y = [addChar (V2 y x) | x <- [0..(xe + 1)]]
--       addChar p
--         | p == s = 'S'
--         | p == e = 'E'
--         | p `S.member` ps = '.'
--         | otherwise = '#'

{------------------------------{ 2nd part }------------------------------}

day16b :: String -> String
day16b =  show . part2 . parseLines . lines


part2 :: Maze -> Int
part2 m = S.size bestSeats
    where
      graph = MkGraph {vertices=convertPoints m.points, neighbours=getNeighbours' m.points}
      source = m.start
      end = m.end
      paths = myDijkstra graph (source, V2 0 1) (isGoal' end)
      bestSeats = S.map fst paths

dirs :: [Direction]
dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

convertPoints :: S.Set Point -> S.Set Node
convertPoints = S.fromList . concatMap (\p -> [(p, d) | d <- dirs]) . S.toList

myDijkstra :: (Num c, Ord c, Hashable v, Ord v) => Graph v c -> v -> (v -> Bool) -> S.Set v
myDijkstra graph start isGoal = backtrace ends
    where
      paths = dijkstraAllShortestPaths queue dist prev graph.neighbours isGoal
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty
      ends = S.fromList . filter isGoal . M.keys $ paths
      backtrace = S.unions . S.map go
        where
          go current
            | current `M.notMember` paths = S.singleton current
            | otherwise = S.insert current (S.unions . S.map go $ paths M.! current)

dijkstraAllShortestPaths :: (Num c, Ord c, Hashable v, Ord v) => H.HashPSQ v c c -> M.Map v c -> M.Map v (S.Set v) -> (v -> [(v, c)]) -> (v -> Bool) -> M.Map v (S.Set v)
dijkstraAllShortestPaths queue dist prev getNeighbours isGoal
    | isGoal current = paths
    | otherwise = dijkstraAllShortestPaths queue' dist' prev' getNeighbours isGoal
    where
      (current, _, distStart, rest) = fromJust . H.minView $ queue
      ngbrs = mapMaybe consider . getNeighbours $ current
      queue' = foldr (\(nbr, cost) -> H.insert nbr cost cost) rest ngbrs
      dist' = foldr (uncurry M.insert) dist ngbrs
      prev' = foldr (M.alter (update current) . fst) prev ngbrs
      update pCurr Nothing = Just . S.singleton $ pCurr
      update pCurr (Just pNgb) = Just . S.insert pCurr $ pNgb
      consider (ngb, dstNgbr)
        | ngb `M.notMember` dist || newDist <= dist M.! ngb = Just (ngb, newDist)
        | otherwise = Nothing
        where
          newDist = distStart + dstNgbr
      paths = foldr M.delete prev . filter ((> bestDist) . (dist M.!)) $ goalNodes
      goalNodes = filter isGoal . M.keys $ dist
      bestDist = minimum . map (dist M.!) $ goalNodes

-- displayDijkstra :: Maze -> Path -> String
-- displayDijkstra (Maze ps s@(V2 ys _) e@(V2 _ xe)) prev = intercalate "\n" [line y | y <- [0..(ys + 1)]]
--     where
--       line y = [addChar (V2 y x) | x <- [0..(xe + 1)]]
--       addChar p
--         | p == s = 'S'
--         | p == e = 'E'
--         | p `S.member` prev = 'O'
--         | p `S.member` ps = '.'
--         | otherwise = '#'
