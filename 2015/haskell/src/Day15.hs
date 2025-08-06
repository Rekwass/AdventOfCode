{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day15 (day15a, day15b) where

import Data.List.Split (splitOneOf)

data Ingredient = Ingredient { name :: String, capacity :: Int, durability :: Int, flavour :: Int, texture :: Int, calories :: Int } deriving (Eq, Show)

day15a :: String -> String
day15a = show . run . parseLines . lines

parseLines :: [String] -> [Ingredient]
parseLines = foldr (parseLines' . splitOneOf ": ,") [] 
    where    
      parseLines' [n,_,_,cap,_,_,d,_,_,f,_,_,t,_,_,cal] acc = Ingredient n (read cap) (read d) (read f) (read t) (read cal) : acc

run :: [Ingredient] -> Int
run is = maximum $ [sumIngredients is [n1,n2,n3,n4] | n1 <- [0..100],
                                                      n2 <- [0..100],
                                                      n3 <- [0..100], 
                                                      n4 <- [0..100],
                                                      n1 + n2 + n3 + n4 == 100] 

sumIngredients :: [Ingredient] -> [Int] -> Int
sumIngredients is ns = caps * durs * flas * texs
    where
      caps = computeProperty capacity is ns
      durs = computeProperty durability is ns
      flas = computeProperty flavour is ns
      texs = computeProperty texture is ns

computeProperty :: (Ingredient -> Int) -> [Ingredient] -> [Int] -> Int
computeProperty acc is ns = max (sum $ zipWith (\i n -> acc i * n) is ns) 0

---------------------------------------------------------------------------
-- Part 2.

day15b :: String -> String
day15b = show . run' . parseLines . lines

run' :: [Ingredient] -> Int
run' is = (fst . maxScore) $ filter (\(_, cal) -> cal == 500) [sumIngredients' is [n1,n2,n3,n4] | n1 <- [0..100], 
                                                                                                  n2 <- [0..100],
                                                                                                  n3 <- [0..100],
                                                                                                  n4 <- [0..100],
                                                                                                  n1 + n2 + n3 + n4 == 100] 

maxScore :: [(Int, Int)] -> (Int, Int)
maxScore = foldr1 (\n@(ns, _) acc@(os, _) -> if ns > os then n else acc)

sumIngredients' :: [Ingredient] -> [Int] -> (Int, Int)
sumIngredients' is ns = (caps * durs * flas * texs, cals)
    where
      caps = computeProperty capacity is ns
      durs = computeProperty durability is ns
      flas = computeProperty flavour is ns
      texs = computeProperty texture is ns
      cals = computeProperty calories is ns
