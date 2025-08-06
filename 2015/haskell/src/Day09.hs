{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 (day09a, day09b) where

import Data.Map (Map, empty, lookup, insertWith, keys)
import Data.List.Split (splitOn)
import Data.List (permutations)
import Data.Maybe (mapMaybe)

newtype Coord = Coord [(String, Int)] deriving (Eq, Show)

instance Semigroup Coord where
    (<>) (Coord xs) (Coord ys) = Coord $ xs ++ ys

day09a :: String -> String
day09a = show . run . initMap . map (splitOn " ") . lines

initMap :: [[String]] -> Map String Coord
initMap = foldr initMap' empty
    where
      -- INFO: Insert in both directions
      initMap' [from,_,to,_,dist] g = insertWith (<>) to (Coord [(from, read dist)]) $ insertWith (<>) from (Coord [(to, read dist)]) g

run :: Map String Coord -> Int
run g = minimum $ mapMaybe (computeLen g) perms
    where
      perms = permutations $ keys g

computeLen :: Map String Coord -> [String] -> Maybe Int
computeLen _ [] = Just 0
computeLen _ [_] = Just 0
computeLen g (from:to:xs) = Data.Map.lookup from g >>= (\crd -> (+) <$> addDst to crd <*> computeLen g (to:xs))

addDst :: String -> Coord -> Maybe Int
addDst city (Coord xs) = foldr findCity Nothing xs
    where
      findCity (c, d) acc
        | city == c = Just d
        | otherwise = acc

---------------------------------------------------------------------------
-- Part 2.

day09b :: String -> String
day09b = show . run' . initMap . map (splitOn " ") . lines

run' :: Map String Coord -> Int
run' g = maximum $ mapMaybe (computeLen g) perms
    where
      perms = permutations $ keys g
