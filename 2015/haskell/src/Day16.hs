{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day16 (day16a, day16b) where

import Data.Map (Map, fromList, intersectionWithKey, elems, intersectionWith)
import Text.Regex.PCRE.Heavy (re, scan)

{------------------------------{ 1st part }------------------------------}

theAunt :: Map String Int
theAunt = parseLine "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

day16a :: String -> String
day16a = show . run . parseLines . lines

parseLines :: [String] -> [Map String Int]
parseLines = map parseLine

parseLine :: String -> Map String Int
-- INFO:                                 Regex with QuasiQuotes. Using PCRE
parseLine line = fromList (map (pair . snd) $ scan [re|(\w+): (\d+)|] line)
    where
      pair [property, count] = (property, read count :: Int)

run :: [Map String Int] -> Int
{-
 INFO: Here is what I did:
 Check for every value in the Map if it is equal or not to the og Aunt
 Then zip each element to assign an id to the aunts
 Finally keep the only one that has every statements set to True
-}
run = fst . head . filter (and . elems . snd) . zip [1..] . map (intersectionWith (==) theAunt)

{------------------------------{ 2nd part }------------------------------}

day16b :: String -> String
day16b = show . run' . parseLines . lines

run' :: [Map String Int] -> Int
run' = fst . head . filter (and . elems . snd) . zip [1..] . map (intersectionWithKey checkClue theAunt)
    where
      checkClue "cats" = (<)
      checkClue "tree" = (<)
      checkClue "pomeranians" = (>)
      checkClue "goldfish" = (>)
      checkClue _ = (==)
