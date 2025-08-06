{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
module Main (main) where

import Day01 (day01a, day01b)
import Day02 (day02a, day02b)
import Day03 (day03a, day03b)
import Day04 (day04a, day04b)
import Day05 (day05a, day05b)
import Day06 (day06a, day06b)
import Day07 (day07a, day07b)
import Day08 (day08a, day08b)
import Day09 (day09a, day09b)
import Day10 (day10a, day10b)
import Day11 (day11a, day11b)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)
import Day14 (day14a, day14b)
import Day15 (day15a, day15b)
import Day16 (day16a, day16b)
import Day17 (day17a, day17b)
import Day18 (day18a, day18b)
import Day19 (day19a, day19b)
import Day20 (day20a, day20b)

import Control.Exception (catch, IOException)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data FilteredArgs = FilteredArgs String String String
data ParsedArgs   = ParsedArs Int Int String

main :: IO ()
main = getArgs >>= filterArgs >>= \(FilteredArgs fday fpart finput) -> checkArgs (readMaybe fday :: Maybe Int) (readMaybe fpart :: Maybe Int) finput >>= \parsedArgs -> runAoc parsedArgs 1

filterArgs :: [String] -> IO FilteredArgs
filterArgs []                 = displayDays >> getLine >>= \day -> displayParts >> getLine >>= \part -> displayInput >> getLine >>= \input -> return (FilteredArgs day part input)
filterArgs [day]              = displayParts >> getLine >>= \part -> displayInput >> getLine >>= \input -> return (FilteredArgs day part input)
filterArgs [day, part]        = displayInput >> getLine >>= \input -> return (FilteredArgs day part input)
filterArgs [day, part, input] = return (FilteredArgs day part input)
-- TODO : Throw when invalid case
filterArgs _                  = return (FilteredArgs "" "" "")

checkArgs :: Maybe Int -> Maybe Int -> String -> IO ParsedArgs
checkArgs Nothing    Nothing     _                        =                                                   return (ParsedArs 0   0    "")
checkArgs Nothing    (Just part) _
    | part == 1 || part == 2                              =                                                   return (ParsedArs 0   part "")
    | otherwise                                           =                                                   return (ParsedArs 0   0    "")
checkArgs (Just day) Nothing     input
    | day >= 1 && day <= 25                               = getFileContentOrElse "" input >>= \fileContent -> return (ParsedArs day 0    fileContent)
    | otherwise                                           =                                                   return (ParsedArs 0   0    "")
checkArgs (Just day) (Just part) input
    | (day >= 1 && day <= 25) && (part == 1 || part == 2) = getFileContentOrElse "" input >>= \fileContent -> return (ParsedArs day part fileContent)
    | (day >= 1 && day <= 25) && (part < 1 || part > 2)   = getFileContentOrElse "" input >>= \fileContent -> return (ParsedArs day 0    fileContent)
    | (day < 1 || day > 25) && (part == 1 || part == 2)   =                                                   return (ParsedArs 0   part "")
    | otherwise                                           =                                                   return (ParsedArs 0   0    "")

getFileContentOrElse :: String -> FilePath -> IO String
getFileContentOrElse empty filePath = readFile filePath `catch`
    \e -> (if filePath /= "" then putStrLn ("File " ++ filePath ++ " not found.") else putStr "") >> const (return empty) (e :: IOException)

displayDays :: IO ()
displayDays = putStrLn "Which day ? [1 - 25]" >>
    putStrLn "--- Day 1:  Not Quite Lisp ---" >>
    putStrLn "--- Day 2:  I Was Told There Would Be No Math ---" >>
    putStrLn "--- Day 3:  Perfectly Spherical Houses in a Vacuum ---" >>
    putStrLn "--- Day 4:  The Ideal Stocking Stuffer ---" >>
    putStrLn "--- Day 5:  Doesn't He Have Intern-Elves For This? ---" >>
    putStrLn "--- Day 6:  Probably a Fire Hazard ---" >>
    putStrLn "--- Day 7:  Some Assembly Required ---" >>
    putStrLn "--- Day 8:  Matchsticks ---" >>
    putStrLn "--- Day 9:  All in a Single Night ---" >>
    putStrLn "--- Day 10: Elves Look, Elves Say ---" >>
    putStrLn "--- Day 11: Corporate Policy ---" >>
    putStrLn "--- Day 12: JSAbacusFramework.io ---" >>
    putStrLn "--- Day 13: Knights of the Dinner Table ---" >>
    putStrLn "--- Day 14: Reindeer Olympics ---" >>
    putStrLn "--- Day 15: Science for Hungry People ---" >>
    putStrLn "--- Day 16: Aunt Sue ---" >>
    putStrLn "--- Day 17: No Such Thing as Too Much ---" >>
    putStrLn "--- Day 18: Like a GIF For Your Yard ---" >>
    putStrLn "--- Day 19: Medicine for Rudolph ---" >>
    putStrLn "--- Day 20: Infinite Elves and Infinite Houses ---" >>
    putStrLn "--- Day 21: RPG Simulator 20XX -" >>
    putStrLn "--- Day 22: Wizard Simulator 20XX ---" >>
    putStrLn "--- Day 23: Opening the Turing Lock ---" >>
    putStrLn "--- Day 24: It Hangs in the Balance ---" >>
    putStrLn "--- Day 25: Let It Snow ---" >>
    prompt "-> "

displayParts :: IO ()
displayParts = putStrLn "Which part ? [1 - 2]" >>
    putStr "--- Part 1 ---\t" >>
    putStrLn "--- Part 2 ---" >>
    prompt "-> "

displayInput :: IO ()
displayInput = putStrLn "Which input ? [defaults to '../puzzle-input/dayX.txt']" >>
    putStr "--- Method 1 : Enter filepath ---\t" >>
    putStrLn "--- Method 2 : Enter puzzle input ---" >>
    prompt "-> "

prompt :: String -> IO ()
prompt x = putStr x >> hFlush stdout

runAoc :: ParsedArgs -> Int -> IO ()
runAoc _ 26 = putStr ""
runAoc parsedArgs@(ParsedArs 0   0    _)           day' = getFileContentOrElse "" ("../puzzle-input/day" ++ show day' ++ ".txt") >>= \fileContent' -> putStrLn ("Day " ++ show day' ++ "\nPart 1 : " ++ runDay day' 1 fileContent' ++ "\nPart 2 : " ++ runDay day' 2 fileContent') >> runAoc parsedArgs (day' + 1)
runAoc parsedArgs@(ParsedArs 0   part _)           day' = getFileContentOrElse "" ("../puzzle-input/day" ++ show day' ++ ".txt") >>= \fileContent' -> putStrLn ("Day " ++ show day' ++ "\nPart " ++ show part ++ " : " ++ runDay day' part fileContent') >> runAoc parsedArgs (day' + 1)
runAoc            (ParsedArs day 0    "")          _    = getFileContentOrElse "" ("../puzzle-input/day" ++ show day ++ ".txt") >>= \fileContent' -> putStrLn ("Day " ++ show day ++ "\nPart 1 : " ++ runDay day 1 fileContent' ++ "\nPart 2 : " ++ runDay day 2 fileContent')
runAoc            (ParsedArs day 0    fileContent) _    = putStrLn ("Day " ++ show day ++ "\nPart 1 : " ++ runDay day 1 fileContent ++ "\nPart 2 : " ++ runDay day 2 fileContent)
runAoc            (ParsedArs day part "")          _    = getFileContentOrElse "" ("../puzzle-input/day" ++ show day ++ ".txt") >>= \fileContent' -> putStrLn ("Day " ++ show day ++ "\nPart " ++ show part ++ " : " ++ runDay day part fileContent')
runAoc            (ParsedArs day part fileContent) _    = putStrLn ("Day " ++ show day ++ "\nPart " ++ show part ++ " : " ++ runDay day part fileContent)

runDay :: Int -> Int -> String -> String
runDay 1    1    fileContent = day01a fileContent
runDay 1    2    fileContent = day01b fileContent
runDay 2    1    fileContent = day02a fileContent
runDay 2    2    fileContent = day02b fileContent
runDay 3    1    fileContent = day03a fileContent
runDay 3    2    fileContent = day03b fileContent
runDay 4    1    fileContent = day04a fileContent
runDay 4    2    fileContent = day04b fileContent
runDay 5    1    fileContent = day05a fileContent
runDay 5    2    fileContent = day05b fileContent
runDay 6    1    fileContent = day06a fileContent
runDay 6    2    fileContent = day06b fileContent
runDay 7    1    fileContent = day07a fileContent
runDay 7    2    fileContent = day07b fileContent
runDay 8    1    fileContent = day08a fileContent
runDay 8    2    fileContent = day08b fileContent
runDay 9    1    fileContent = day09a fileContent
runDay 9    2    fileContent = day09b fileContent
runDay 10   1    fileContent = day10a fileContent
runDay 10   2    fileContent = day10b fileContent
runDay 11   1    fileContent = day11a fileContent
runDay 11   2    fileContent = day11b fileContent
runDay 12   1    fileContent = day12a fileContent
runDay 12   2    fileContent = day12b fileContent
runDay 13   1    fileContent = day13a fileContent
runDay 13   2    fileContent = day13b fileContent
runDay 14   1    fileContent = day14a fileContent
runDay 14   2    fileContent = day14b fileContent
runDay 15   1    fileContent = day15a fileContent
runDay 15   2    fileContent = day15b fileContent
runDay 16   1    fileContent = day16a fileContent
runDay 16   2    fileContent = day16b fileContent
runDay 17   1    fileContent = day17a fileContent
runDay 17   2    fileContent = day17b fileContent
runDay 18   1    fileContent = day18a fileContent
runDay 18   2    fileContent = day18b fileContent
runDay 19   1    fileContent = day19a fileContent
runDay 19   2    fileContent = day19b fileContent
runDay 20   1    fileContent = day20a fileContent
runDay 20   2    fileContent = day20b fileContent
runDay day part _           = "day " ++ show day ++ " part " ++ show part ++ " is not implemented yet."
