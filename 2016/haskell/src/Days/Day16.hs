{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day16 (day16a, day16b) where

import Data.List.Split (chunksOf)

{------------------------------{ 1st part }------------------------------}

day16a :: String -> String
day16a = show . part1 . head . lines

part1 :: String -> String
part1 = computeCheckSum . take diskLen . head . filter ((>= diskLen). length) . iterate dragonCurve
  where
    diskLen = 272

dragonCurve :: String -> String
dragonCurve a = a ++ "0" ++ b''
  where
    b = a
    b' = reverse b
    b'' = map (\c -> if c == '0' then '1' else '0') b'

computeCheckSum :: String -> String
computeCheckSum x
  | odd (length x') = x'
  | otherwise = computeCheckSum x'
  where
    xs = chunksOf 2 x
    x' = map (\[a,b] -> if a == b then '1' else '0') xs

{------------------------------{ 2nd part }------------------------------}

day16b :: String -> String
day16b = show . part2 . head . lines

part2 :: String -> String
part2 = computeCheckSum . take diskLen . head . filter ((>= diskLen). length) . iterate dragonCurve
  where
    diskLen = 35651584
