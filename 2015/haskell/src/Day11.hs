module Day11 (day11a, day11b) where
import Data.List (group, find)
import Data.Maybe (fromMaybe)

day11a :: String -> String
day11a = run . head . lines

run :: String -> String
run = repeatWhileFalse validatePassword increasePassword

repeatWhileFalse :: (String -> Bool) -> (String -> String) -> String -> String
repeatWhileFalse cond f x = fromMaybe "" $ find cond (iterate f x)

validatePassword :: String -> Bool
validatePassword x = threeSucc x && noIOL x && twoPairs x

increasePassword :: String -> String
increasePassword x = reverse $ (`increasePassword'` True) $ reverse x

increasePassword' :: String -> Bool -> String
increasePassword' [] _ = []
increasePassword' ('z':xs) True = 'a' : increasePassword' xs True
increasePassword' (x:xs) True = succ x : increasePassword' xs False
increasePassword' xs False = xs

threeSucc :: String -> Bool
threeSucc (x:y:z:xs) = y == succ x && z == succ y || threeSucc (y:z:xs)
threeSucc _ = False

noIOL :: String -> Bool
noIOL = foldr (\x acc -> x `notElem` "iol" && acc) True

twoPairs :: String -> Bool
twoPairs = (>=2) . length . filter ((>=2) . length) . group

---------------------------------------------------------------------------
-- Part 2.

day11b :: String -> String
day11b = run . increasePassword . day11a
