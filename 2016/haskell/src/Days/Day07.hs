{-# OPTIONS_GHC -Wno-type-defaults #-}

module Days.Day07 (day07a, day07b) where

import Data.List.Split (splitOn)
import Data.List (partition)
import Data.List.Utils (hasAny)

{------------------------------{ 1st part }------------------------------}

day07a :: String -> String
day07a = show . part1 . lines

part1 :: [String] -> Int
part1 = length . filter id . map isIPV7

isIPV7 :: String -> Bool
isIPV7 x = any hasPair l && not (any hasPair r)
  where
    (l, r) = splitOnSB x

hasPair :: String -> Bool
hasPair (w:x:y:z:xs)
  | w /= x && w == z && x == y = True
  | otherwise                  = hasPair (x:y:z:xs)
hasPair _ = False

splitOnSB :: String -> ([String], [String])
splitOnSB xs = (map snd l, map snd r)
  where
    ps     = concatMap (splitOn "]") $ splitOn "[" xs
    (l, r) = partition (even . fst) $ zip [0..] ps

{------------------------------{ 2nd part }------------------------------}

day07b :: String -> String
day07b = show . part2 . lines

part2 :: [String] -> Int
part2 = length . filter id . map isSSL

isSSL :: String -> Bool
isSSL x = hasAny babl babr
  where
    (l, r) = splitOnSB x
    babl   = concatMap toBAB l
    babr   = concatMap toABA r

toABA :: String -> [String]
toABA x'@(x:y:z:xs)
  | isABA x'  = [x,y,x] : toABA (y:z:xs)
  | otherwise = toABA (y:z:xs)
toABA _       = []

toBAB :: String -> [String]
toBAB x'@(x:y:z:xs)
  | isABA x'  = [y,x,y] : toBAB (y:z:xs)
  | otherwise = toBAB (y:z:xs)
toBAB _       = []

isABA :: String -> Bool
isABA (x:y:z:_) = x == z && x /= y
isABA _         = error "How ???"
