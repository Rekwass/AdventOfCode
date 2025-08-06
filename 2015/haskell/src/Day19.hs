{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day19 (day19a, day19b) where
import Data.List.Split (splitOn)
import Data.List (nub)

{------------------------------{ 1st part }------------------------------}

data Machine = Machine Molecule [(Molecule, Molecule)] deriving (Eq, Show)
type Molecule = String

day19a :: String -> String
day19a = show . part1 . parseLines . lines

parseLines :: [String] -> Machine
parseLines ls = Machine (last ls) (map parseLine (init . init $ ls))

parseLine :: String -> (Molecule, Molecule)
parseLine l = (name, transfo)
    where [name, transfo] = splitOn " => " l

part1 :: Machine -> Int 
part1 (Machine m ms) = length $ nub $ update m ms

update :: Molecule -> [(Molecule, Molecule)] -> [Molecule]
update _ [] = [] 
update m (mol:ms) = replace m "" mol ++ update m ms

replace :: Molecule -> Molecule -> (Molecule, Molecule) -> [Molecule]
replace [] _ _ = []
replace [x] s (from, to)
    | [x] == from = [s ++ to]
    | otherwise = []
replace (x:y:xs) s mol@(from, to)
    | [x,y] == from = (s ++ to ++ xs) : replace xs (s ++ [x,y]) mol
    | [x] == from = (s ++ to ++ [y] ++ xs) : replace (y:xs) (s ++ [x]) mol
    | otherwise = replace (y:xs) (s ++ [x]) mol


{------------------------------{ 2nd part }------------------------------}

{--

As in compilation courses I will use transformations.

First of all a molecule transforms into something.

There is a pattern, it is not random.

We see that `Rn`, `Y` and `Ar` does not transform into anything else, their constants.

There is always `Rn` first when there is an `Ar` later.

Sometimes a `Y` (or two) stands between the `Rn` and `Ar`.

We can say that `Rn` can be converted to a `(`, `Y` to a `,` and `Ar` to a `)`

Using this we can easily list every possible molecule transformation:

X => XX | X(X) | X(X,X) | X(X,X,X)
     -2    -3      -5       -7

So, we need to get the total number of molecules (in my case 292), the number of parenthesis (in my case 72) and the number of commas (in my case 6)

If we remove the number of opening and closing parenthesis, and also remove the number of commas (with the molecules that comes with it), we only have the XX transformations.

In order to count the number of transformations from "ABCDE" to "X" we get the length of "ABCDE" - 1

"ABCDE" => "XCDE" => "XDE" => "XE" => "X"
4 steps, so length of mol - 1 equals number of transformations.

Then we look at our input and compute manually the result

nb mol - parenthesis - commas (and bonus mols) - the -1 from before = mathematical minimum
  292  -     72      -           6 * 2         -          1         =         207

Below are some methods I wrote to help writing this solution

--}

day19b :: String -> String
day19b _ = "Solved manually, check the code" ++ "\nNumber of molecules = " ++ show (countMolecules input) ++ "\nNumber of const molecules Rn '(' and Ar ')' = " ++ show ((length . filter (`elem` "()")) $ replaceConst input) ++ "\nNumber of const molecule Y ',' = " ++ show ((length . filter (== ',')) $ replaceConst input)

input :: String
input = "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"

molecules :: [String]
molecules = ["Al", "B", "Ca", "F", "H", "Mg", "N", "O", "P", "Si", "Th", "Ti", "Rn", "Y", "Ar"]

replaceConst :: String -> String
replaceConst [] = ""
replaceConst (x:y:ys)
    | x == 'Y' = "," ++ replaceConst (y:ys)
    | [x,y] == "Rn" = '(' : replaceConst ys
    | [x,y] == "Ar" = ')' : replaceConst ys
    | otherwise =  x : replaceConst (y:ys)
replaceConst (x:xs)
    | x == 'Y' = "Y" ++ replaceConst xs
    | otherwise = x : replaceConst xs

countMolecules :: String -> Int
countMolecules [] = 0
countMolecules (x:y:ys)
    | [x] `elem` molecules || [x,y] `elem` molecules = 1 + countMolecules (y:ys)
    | otherwise = countMolecules (y:ys)
countMolecules (x:xs)
    | [x] `elem` molecules = 1 + countMolecules xs
    | otherwise = 0
