{-# LANGUAGE TemplateHaskell #-}
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
import Day21 (day21a, day21b)
import Day22 (day22a, day22b)

import System.Environment (getArgs)

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=), zoom)

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A

import Brick.Types (BrickEvent(..))
import Brick.Widgets.Core (str)
import Brick.Util (on, bg)
import qualified Brick as C
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Border (borderWithLabel)

import System.IO.Unsafe (unsafePerformIO)
import MyDialog

data Name =
      DayOneBtn
    | DayTwoBtn
    | DayThreeBtn
    | DayFourBtn
    | DayFiveBtn
    | DaySixBtn
    | DaySevenBtn
    | DayEightBtn
    | DayNineBtn
    | DayTenBtn
    | DayElevenBtn
    | DayTwelveBtn
    | DayThirteenBtn
    | DayFourteenBtn
    | DayFifteenBtn
    | DaySixteenBtn
    | DaySeventeenBtn
    | DayEighteenBtn
    | DayNineteenBtn
    | DayTwentyBtn
    | DayTwentyOneBtn
    | DayTwentyTwoBtn
    | DayTwentyThreeBtn
    | DayTwentyFourBtn
    | DayTwentyFiveBtn
    | PartOneBtn
    | PartTwoBtn
    deriving (Show, Eq, Ord)

type Args = (Day, Part, InputPath)
type Day = Int
type Part = Int
type InputPath = String
type Input = String

data St = St { _day :: Dialog Int Name
             , _part :: Dialog Int Name
             , _step :: Step}

data Step = ChooseDay | ChoosePart Int | Done Int Int deriving (Eq, Show)

makeLenses ''St

main :: IO ()
main = do
  args <- getArgs
  let mArgs = filterArgs args
  case mArgs of
    (Just (d, p, path)) -> do 
      filepath <- readFile path
      putStrLn $ aoc d p filepath
    Nothing -> do
      _ <- M.defaultMain theApp initialState
      return ()

filterArgs :: [String] -> Maybe Args
filterArgs [d, p, path]      = Just (read d, read p, path)
filterArgs _                 = Nothing

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

drawUI :: St -> [T.Widget Name]
drawUI st = 
  case _step st of
    ChooseDay      -> [dayLayer]
    ChoosePart _   -> [partLayer]
    Done       d p -> [doneLayer d p]
    where
      dayLayer = renderDialog (st^.day)
      partLayer = renderDialog (st^.part)
      doneLayer d p = C.centerLayer $ C.withDefAttr dialogAttr $ C.hLimit 50 $ C.vLimit 5 $ C.joinBorders $ C.withBorderStyle unicode $ borderWithLabel (str $ "{ Day " <> show d <> ", part " <> show p <> " }") (C.center $ C.withDefAttr buttonAttr (str $ " " <> aoc d p (unsafePerformIO (readFile ("../inputs/day" <> show d <> ".txt"))) <> " "))
      -- TODO: Find another way to compute than running unsafePerformIO in the UI part...

appEvent :: BrickEvent Name e -> T.EventM Name St ()
appEvent (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> do
          s <- use step
          case s of
            ChooseDay              -> onSelect day  $ \(_, val) -> step .= ChoosePart val
            ChoosePart chosenDay   -> onSelect part $ \(_, val) -> step .= Done chosenDay val
            Done       _         _ -> M.halt
        V.EvKey V.KBS [] -> do
          s <- use step
          case s of
            ChooseDay              -> return ()
            ChoosePart _           -> step .= ChooseDay
            Done       chosenDay _ -> step .= ChoosePart chosenDay
        _ -> do
          s <- use step
          case s of
            ChooseDay      -> zoom day  $ handleDialogEvent e
            ChoosePart _   -> zoom part $ handleDialogEvent e
            Done       _ _ -> M.halt
  where
    onSelect field action = do
      res <- dialogSelection <$> use field
      maybe (return ()) action res
appEvent _ = return ()

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (dialogAttr, V.white `on` V.green)
    , (buttonAttr, V.black `on` V.white)
    , (buttonSelectedAttr, bg V.yellow)
    ]

initialState :: St
initialState = St { _day = d, _part = p, _step = ChooseDay }
  where
    d = dialog (str "{ Select a day! }") (Just (DayOneBtn, choicesDay)) 50
    p = dialog (str "{ Select a part! }") (Just (PartOneBtn, choicesPart)) 50
    choicesDay = [ ("01", DayOneBtn        , 1)
                 , ("02", DayTwoBtn        , 2)
                 , ("03", DayThreeBtn      , 3)
                 , ("04", DayFourBtn       , 4)
                 , ("05", DayFiveBtn       , 5)
                 , ("06", DaySixBtn        , 6)
                 , ("07", DaySevenBtn      , 7)
                 , ("08", DayEightBtn      , 8)
                 , ("09", DayNineBtn       , 9)
                 , ("10", DayTenBtn        , 10)
                 , ("11", DayElevenBtn     , 11)
                 , ("12", DayTwelveBtn     , 12)
                 , ("13", DayThirteenBtn   , 13)
                 , ("14", DayFourteenBtn   , 14)
                 , ("15", DayFifteenBtn    , 15)
                 , ("16", DaySixteenBtn    , 16)
                 , ("17", DaySeventeenBtn  , 17)
                 , ("18", DayEighteenBtn   , 18)
                 , ("19", DayNineteenBtn   , 19)
                 , ("20", DayTwentyBtn     , 20)
                 , ("21", DayTwentyOneBtn  , 21)
                 , ("22", DayTwentyTwoBtn  , 22)
                 , ("23", DayTwentyThreeBtn, 23)
                 , ("24", DayTwentyFourBtn , 24)
                 , ("25", DayTwentyFiveBtn , 25)
                 ]
    choicesPart = [ ("1", PartOneBtn      , 1)
                  , ("2", PartTwoBtn      , 2)
                  ]

aoc :: Day -> Part -> Input -> String
aoc 1  1 = day01a
aoc 1  2 = day01b
aoc 2  1 = day02a
aoc 2  2 = day02b
aoc 3  1 = day03a
aoc 3  2 = day03b
aoc 4  1 = day04a
aoc 4  2 = day04b
aoc 5  1 = day05a
aoc 5  2 = day05b
aoc 6  1 = day06a
aoc 6  2 = day06b
aoc 7  1 = day07a
aoc 7  2 = day07b
aoc 8  1 = day08a
aoc 8  2 = day08b
aoc 9  1 = day09a
aoc 9  2 = day09b
aoc 10 1 = day10a
aoc 10 2 = day10b
aoc 11 1 = day11a
aoc 11 2 = day11b
aoc 12 1 = day12a
aoc 12 2 = day12b
aoc 13 1 = day13a
aoc 13 2 = day13b
aoc 14 1 = day14a
aoc 14 2 = day14b
aoc 15 1 = day15a
aoc 15 2 = day15b
aoc 16 1 = day16a
aoc 16 2 = day16b
aoc 17 1 = day17a
aoc 17 2 = day17b
aoc 18 1 = day18a
aoc 18 2 = day18b
aoc 19 1 = day19a
aoc 19 2 = day19b
aoc 20 1 = day20a
aoc 20 2 = day20b
aoc 21 1 = day21a
aoc 21 2 = day21b
aoc 22 1 = day22a
aoc 22 2 = day22b
aoc _  _ = const "Not implemented yet!"
