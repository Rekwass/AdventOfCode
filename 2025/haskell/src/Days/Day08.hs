{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Days.Day08 (day08a, day08b) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (tails, partition, sortBy)
import Data.Ord (Down(Down), comparing)

{------------------------------{ 1st part }------------------------------}

type Rect = (Int, Int, Int)
type Circuit = S.Set Rect

day08a :: String -> String
day08a = show . part1 . parseLines . lines

parseLines :: [String] -> [Circuit]
parseLines = map (\l -> let [sx,sy,sz] = splitOn "," l in S.fromList [(read sx, read sy, read sz)])

part1 :: [Circuit] -> Int
part1 cs = product . take 3 . sortBy (comparing Down) . map length $ res
  where
    steps = iterate step (cs, dists cs)
    res = fst $ steps !! 1000

dists :: [Circuit] -> [(Rect, Rect, Double)]
dists cs = sortBy (\(_, _, d1) (_, _, d2) -> compare d1 d2) [(l, r, euclidian3D l r) | (l:rest) <- tails rs, r <- rest]
  where
    rs = concatMap S.toList cs

step :: ([Circuit], [(Rect, Rect, Double)]) -> ([Circuit], [(Rect, Rect, Double)])
step (cs, (l, r, _):xs) = (cs':rs, xs)
  where
    (ls, rs) = partition (\c -> l `S.member` c || r `S.member` c) cs
    cs' = updateCircuit ls

updateCircuit :: [Circuit] -> Circuit
updateCircuit [x] = x
updateCircuit [x,y] = S.union x y

euclidian3D :: Rect -> Rect -> Double
euclidian3D (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral (((x1 - x2) ^ 2) + ((y1 - y2) ^ 2) + ((z1 - z2) ^ 2))

{------------------------------{ 2nd part }------------------------------}

day08b :: String -> String
day08b = show . part2 . parseLines . lines

part2 :: [Circuit] -> Int
part2 cs = x1 * x2
  where
    steps = iterate step (cs, dists cs)
    res = head $ dropWhile (\((ls, _), (rs, _)) -> length ls > 1 && length rs /= 1) $ zip steps (tail steps)
    ((x1, _, _), (x2, _, _), _) = head . snd . fst $ res
