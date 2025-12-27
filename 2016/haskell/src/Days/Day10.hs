{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day10 (day10a, day10b) where

{------------------------------{ 1st part }------------------------------}

import Data.List (foldl', sort)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Text (Parser)

-- 0, 1 or 2 values
type Values = [Int]
type Robots = M.Map Id Values

data Id = Robot Int | Output Int deriving (Eq, Ord, Show)

--                  IdL IdH
type Instruction = (Id, Id)
type Instructions = M.Map Id Instruction

data Line' = Val (Int, Id) | Trade (Id, Id, Id)

day10a :: String -> String
day10a = show . part1 . parseLines . T.lines . T.pack

parseLines :: [T.Text] -> (Robots, Instructions)
parseLines = foldl' step (M.empty, M.empty) . map parseLine
  where
    parseLine = either (error . show) id . parse lineParser ""
    step (rs, is) = \case
      Val   (val, botId)    -> (M.insertWith (<>) botId [val] rs, is)
      Trade (botId, bl, bh) -> (rs, M.insert botId (bl, bh) is)

lineParser :: Parser Line'
lineParser = try (Val <$> valueParser) <|> Trade <$> tradeParser

valueParser :: Parser (Int, Id)
valueParser = (,)
  <$> (string "value " *> number)
  <*> (string " goes to bot " *> (Robot <$> number))

tradeParser :: Parser (Id, Id, Id)
tradeParser = (,,)
  <$> (string "bot " *> (Robot <$> number))
  <*> (string " gives low to " *> idParser)
  <*> (string " and high to " *> idParser)

idParser :: Parser Id
idParser =
  try (string "bot "    *> (Robot <$> number))
  <|>  string "output " *> (Output <$> number)

number :: Parser Int
number = read <$> many1 digit

part1 :: (Robots, Instructions) -> Int
part1 = getHim . fst . head . dropWhile notCmp61and17 . iterate step
  where
    step (rs, is) = (foldr (updateRobot is) rs (twoValRobots rs), is)

twoValRobots :: Robots -> [Id]
twoValRobots = M.keys . M.filter ((==2) . length)

updateRobot :: Instructions -> Id -> Robots -> Robots
updateRobot is botId rs =
    M.insert botId []
  . M.insertWith (<>) bl [minimum val]
  . M.insertWith (<>) bh [maximum val]
  $ rs
  where
    (bl, bh) = is M.! botId
    val      = M.findWithDefault [] botId rs

getHim :: Robots -> Int
getHim = (\(Robot botId) -> botId) . head . M.keys . M.filterWithKey isGood

notCmp61and17 :: (Robots, Instructions) -> Bool
notCmp61and17 (rs, _) = M.null $ M.filterWithKey isGood rs

isGood :: Id -> Values -> Bool
isGood (Output _) _  = False
isGood (Robot _)  ns = sort ns == [17, 61]

{------------------------------{ 2nd part }------------------------------}

day10b :: String -> String
day10b = show . part2 . parseLines . T.lines . T.pack

part2 :: (Robots, Instructions) -> Int
part2 = get012 . fst . head . dropWhile robotHas2val . iterate step
  where
    robotHas2val = any ((==2) . length) . fst
    step (rs, is) = (foldr (updateRobot is) rs (twoValRobots rs), is)

get012 :: Robots -> Int
get012 rs = product $ concat [rs M.! Output i | i <- [0..2]]
