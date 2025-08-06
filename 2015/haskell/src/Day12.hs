module Day12 (day12a, day12b) where

import Data.Aeson (Value (Object, Array, Number, String), decode)
import Data.Aeson.KeyMap (toList)
import Data.Scientific (toBoundedInteger)
import Data.Text (pack)
import Data.String (IsString(fromString))
import Data.Maybe (fromJust)

day12a :: String -> String
day12a = show . run . fromJust . decode . fromString

run :: Value -> Int
run = fromJust . toBoundedInteger . addNumbers
  where
    addNumbers (Object obj) = foldr (\(_, v) acc -> acc + addNumbers v) 0 $ toList obj
    addNumbers (Array arr) = foldr (\v acc -> acc + addNumbers v) 0 arr
    addNumbers (Number num) = num
    addNumbers _ = 0

---------------------------------------------------------------------------
-- Part 2.

day12b :: String -> String
day12b = show . run' . fromJust . decode . fromString

run' :: Value -> Int
run' = fromJust . toBoundedInteger . addNumbers'
    where
      addNumbers' (Object obj)
        | String (pack "red") `elem` obj = 0
        | otherwise = foldr (\(_, v) acc -> acc + addNumbers' v) 0 $ toList obj 
      addNumbers' (Array arr) = foldr (\v acc -> acc + addNumbers' v) 0 arr
      addNumbers' (Number num) = num
      addNumbers' _ = 0
