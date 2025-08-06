{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day05 (day05a, day05b) where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)

type Page = Int
type Updates = [Page]

{------------------------------{ 1st part }------------------------------}

day05a :: String -> String
day05a = show . part1 . parseLines . lines

parseLines :: [String] -> (M.Map Page [Page], [Updates])
parseLines ls = (M.fromListWith (++) ips, ius)
    where
      (ps, us) = break null ls
      ips = map (toTuple . map read . splitOn "|") ps
      toTuple [x,y] = (x,[y])
      ius = map (map read . splitOn ",") $ tail us

part1 :: (M.Map Int [Int], [Updates]) -> Int
part1 (m, us) = sum . map (middleElement . reverse) $ filter (isCorrectOrder m) $ map reverse us

middleElement :: [a] -> a
middleElement xs = xs !! (length xs `div` 2)

isCorrectOrder :: M.Map Page [Page] -> Updates -> Bool
isCorrectOrder _ [] = True
isCorrectOrder m (u:us)
  | isNothing $ M.lookup u m = isCorrectOrder m us
  | any (`elem` us) (fromJust $ M.lookup u m) = False
  | otherwise = isCorrectOrder m us

{------------------------------{ 2nd part }------------------------------}

day05b :: String -> String
day05b = show . part2 . parseLines . lines

part2 :: (M.Map Int [Int], [Updates]) -> Int
part2 (m, us) = sum (map (middleElement . fixOrder m . reverse) . filter (not . isCorrectOrder m) $ map reverse us)

fixOrder :: M.Map Page [Page] -> Updates -> Updates
fixOrder m = foldr (insertPage m) []

insertPage :: M.Map Page [Page] -> Page -> Updates -> Updates
insertPage _ p [] = [p]
insertPage m p (x:xs)
    | M.notMember p m = (x:xs) ++ [p]
    | x `elem` fromJust (M.lookup p m) = p : (x:xs)
    | otherwise = x : insertPage m p xs
