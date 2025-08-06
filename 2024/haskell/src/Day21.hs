module Day21 (day21a, day21b) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashPSQ as H
import Linear (V2 (V2))
import Algorithm (dijkstraAllShortestPaths)
import Data.Hashable (Hashable)
import Data.MemoTrie (memoFix)

type Point = V2 Int
type Cost = Int

numericKeypad :: M.Map Point Char
numericKeypad = M.fromList [(V2 0 0, '7'), (V2 0 1, '8'), (V2 0 2, '9'),
                            (V2 1 0, '4'), (V2 1 1, '5'), (V2 1 2, '6'),
                            (V2 2 0, '1'), (V2 2 1, '2'), (V2 2 2, '3'),
                                           (V2 3 1, '0'), (V2 3 2, 'A')]

pathTable :: Char -> Char -> [String]
pathTable '<' '<' = ["A"]
pathTable '<' '^' = [">^A"]
pathTable '<' 'v' = [">A"]
pathTable '<' '>' = [">>A"]
pathTable '<' 'A' = [">>^A", ">^>A"]

pathTable '^' '<' = ["v<A"]
pathTable '^' '^' = ["A"]
pathTable '^' 'v' = ["vA"]
pathTable '^' '>' = [">vA", "v>A"]
pathTable '^' 'A' = [">A"]

pathTable 'v' '<' = ["<A"]
pathTable 'v' '^' = ["^A"]
pathTable 'v' 'v' = ["A"]
pathTable 'v' '>' = [">A"]
pathTable 'v' 'A' = [">^A", "^>A"]

pathTable '>' '<' = ["<<A"]
pathTable '>' '^' = ["^<A", "<^A"]
pathTable '>' 'v' = ["<A"]
pathTable '>' '>' = ["A"]
pathTable '>' 'A' = ["^A"]

pathTable 'A' '<' = ["v<<A", "<v<A"]
pathTable 'A' '^' = ["<A"]
pathTable 'A' 'v' = ["v<A", "<vA"]
pathTable 'A' '>' = ["vA"]
pathTable 'A' 'A' = ["A"]
pathTable _ _ = error "how"

{------------------------------{ 1st part }------------------------------}

day21a :: String -> String
day21a = show . part1 . lines

part1 :: [String] -> Int
part1 = sum . map (bipBoop 3)

bipBoop :: Int -> String -> Int
bipBoop depth code = minimum res * num
  where
  numericKeypadPaths = findNumericKeypadPaths code
  res = map (computePathCost mem depth) numericKeypadPaths
  mem = memoFix moveCost
  num = read $ init code :: Int

findNumericKeypadPaths :: String -> [String]
findNumericKeypadPaths code = pathPermutations chars
  where
    inputs = zipWith (findPath numericKeypad) ('A':code) code
    chars =  map (map convertToChar) inputs

findPath :: M.Map Point Char -> Char -> Char -> [[Point]]
findPath keyPad c c' = myDijkstraAllPath (findKeyByValue c keyPad) (getNeighbours' keyPad) (== findKeyByValue c' keyPad)

getNeighbours' :: M.Map Point Char -> Point -> [(Point, Cost)]
getNeighbours' ps p = [(p', 1) | d <- dirs, let p' = p + d, p' `M.member` ps]
  where
    dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

myDijkstraAllPath :: (Num c, Ord c, Hashable v, Ord v) => v -> (v -> [(v , c)]) -> (v -> Bool) -> [[v]]
myDijkstraAllPath start neighbours isGoal
  | isGoal start = [[]]
  | otherwise = backtrace end
    where
      paths = dijkstraAllShortestPaths queue dist prev neighbours isGoal
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty
      end = head . filter isGoal . M.keys $ paths
      backtrace current = case M.lookup current paths of
        Nothing -> [[current]]
        Just parents -> [current : path | parent <- S.toList parents, path <- backtrace parent]

convertToChar :: [Point] -> [Char]
convertToChar ps = (++ ['A']) $ map (\(p, p') -> case p' - p of
        V2 (-1) 0 -> '^'
        V2 0 1 -> '>'
        V2 1 0 -> 'v'
        _ -> '<'
      ) pss
  where
    pss = zip rps (tail rps)
    rps = reverse ps

pathPermutations :: [[String]] -> [String]
pathPermutations = foldr (\xs xss -> [x ++ xs' | x <- xs, xs' <- xss]) [""]

findKeyByValue :: Char -> M.Map Point Char -> Point
findKeyByValue val mp = head $ [k | (k, v) <- M.toList mp, v == val]

computePathCost :: ((Int, Char, Char) -> Int) -> Int -> String -> Int
computePathCost _ 1     path = length path
computePathCost rec depth path = sum $ map (\(a, b) -> rec (depth,a,b)) pathMoves
  where
    aPath = 'A':path
    pathMoves = zip aPath $ tail aPath

moveCost :: ((Int, Char, Char) -> Int) -> (Int, Char, Char) -> Int
moveCost rec (depth, s , e) = minimum $ map (computePathCost rec (depth - 1)) (pathTable s e)

{------------------------------{ 2nd part }------------------------------}

day21b :: String -> String
day21b =  show . part2 . lines

part2 :: [String] -> Int
part2 = sum . map (bipBoop 26)
