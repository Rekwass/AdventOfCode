module Day08 (day08a, day08b) where

day08a :: String -> String
day08a = show . getRes . run . lines

getRes :: (Int, Int) -> Int
getRes (x,y) = x - y

run :: [String] -> (Int, Int)
run = foldr countThings (0, 0)

countThings :: String -> (Int, Int) -> (Int, Int) 
countThings x (cl, cm) = (ncl, ncm)
    where
        ncl = cl + length x
        ncm = cm + length (decode x) - 2

decode :: String -> String
decode [] = []
decode ('\\':'x':_:_:xs) = '@' : decode xs
decode ('\\':x:xs) = x : decode xs
decode (x:xs) = x: decode xs

---------------------------------------------------------------------------
-- Part 2.

day08b :: String -> String
day08b = show . getRes . run' . lines

run' :: [String] -> (Int, Int)
run' = foldr countThings' (0, 0)

countThings' :: String -> (Int, Int) -> (Int, Int) 
countThings' x (cl, cm) = (ncl, ncm)
    where
        ncl = cl + length ("\\\"" ++ encode x ++ "\\\"")
        ncm = cm + length x

encode :: String -> String
encode [] = []
encode ('\\':'x':xs) = "\\\\x" ++ encode xs
encode ('\\':x:xs) =  "\\\\\\" ++ x : encode xs
encode (x:xs) = x : encode xs
