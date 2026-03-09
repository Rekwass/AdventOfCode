module Days.Day12 (day12a, day12b) where
import Data.List.Split (splitOn)

{------------------------------{ 1st part }------------------------------}

import qualified Data.Set as S

type Point = (Int, Int)
type Piece = S.Set Point
type Area = (Point, [Int])

day12a :: String -> String
day12a = show . part1 . parseLines

parseLines :: String -> ([Piece], [Area])
parseLines line = (ps, as)
  where
    xs = splitOn "\n\n" line
    ps = map (parsePiece . tail . splitOn "\n") $ init xs
    as = map parseArea . init . splitOn "\n" . last $ xs

parsePiece :: [String] -> Piece
parsePiece ls = S.fromList $ concat [[(y, x) | ('#', x) <- zip l [0..]] | (l, y) <- zip ls [0..]]

parseArea :: String -> Area
parseArea line = ((y, x), nums)
  where
    [l, r] = splitOn ": " line
    [x,y] = map read $ splitOn "x" l
    nums = map read $ words r

part1 :: ([Piece], [Area]) -> Int
part1 (ps, as) = length . filter (> 0) $ possible
  where
    pareas = map S.size ps
    possible = map (\((y,x), ips) -> (y*x) - (sum $ map (\(n, pa) -> n * pa) (zip ips pareas))) as
    
{------------------------------{ 2nd part }------------------------------}

day12b :: String -> String
day12b = show . part2

part2 :: String -> String
part2 = const "No part 2"
