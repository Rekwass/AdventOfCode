{-# LANGUAGE QuasiQuotes #-}

module Days.Day04 (day04a, day04b) where

import Data.List.Split (splitOn)
import Data.Function (on)
import Text.Regex.PCRE.Heavy (re, scan)
import Data.List (group, sort, sortBy, find, isPrefixOf)
import Lib.Cipher (caesar)
import Data.Maybe (fromJust)

{------------------------------{ 1st part }------------------------------}

type Room = ([String], Int, String) 

day04a :: String -> String
day04a = show . part1 . parseLines . lines

parseLines :: [String] -> [Room]
parseLines = map parseLine
  where
    parseLine line =
      let parts = splitOn "-" line
          [n, s] = snd . head $ scan [re|(\d+)\[([a-z]{5})\]|] (last parts)
          letters = concat (init parts)
      in (sortBy (flip compare `on` length) $ group $ sort letters, read n, s)

part1 :: [Room] -> Int
part1 = sum . map getNum . filter isRealRoom
  where
    getNum (_, n, _) = n

isRealRoom :: Room -> Bool
isRealRoom (xs, _, name) = all (uncurry (==)) $ zip (map head xs) name

{------------------------------{ 2nd part }------------------------------}

type Room' = (String, Int)

day04b :: String -> String
day04b = show . part2 . parseLines' . lines

parseLines' :: [String] -> [Room']
parseLines' = map parseLine
  where
    parseLine line =
      let ws = splitOn "-" line
          [n, _] = snd . head $ scan [re|(\d+)\[([a-z]{5})\]|] (last ws)
      in (unwords $ init ws, read n)

part2 :: [Room'] -> Int
part2 rs = snd . fromJust . find (("north" `isPrefixOf`) . fst) $ [(caesar n name , n) | (name, n) <- rs]
