module Day01 (day01a, day01b) where

day01a :: String -> String
day01a = show . getFloor

day01b :: String -> String
day01b = show . getBasementFloor

getFloor :: String -> Int
getFloor fileContent = getFloor' fileContent 0

getFloor' :: String -> Int -> Int
getFloor' [] floorLevel = floorLevel
getFloor' (x : xs) floorLevel
  | x == '(' = getFloor' xs floorLevel + 1
  | x == ')' = getFloor' xs floorLevel - 1
  | otherwise = getFloor' xs floorLevel

getBasementFloor :: String -> Int
getBasementFloor fileContent = getBasementFloor' fileContent 0 0

getBasementFloor' :: String -> Int -> Int -> Int
getBasementFloor' [] _ index = index
getBasementFloor' (x : xs) floorLevel index
  | floorLevel == -1 = index
  | x == '(' = getBasementFloor' xs (floorLevel + 1) index + 1
  | x == ')' = getBasementFloor' xs (floorLevel - 1) index + 1
  | otherwise = getBasementFloor' xs floorLevel index + 1
