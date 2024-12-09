module Lib
    ( DayNum
    , Answer
    , Day (..)
    , days
    ) where

import Paths_aoc24
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Day1
import Day2
import Day3
import Day4
import Day5

import Day7
import Day8

type DayNum = String

type Answer = (String, String)

data Day = Day { runDay :: IO Answer
               , numDay :: DayNum }

liftDay :: DayNum -> (Text -> Answer) -> IO Answer
liftDay daynum day = do
  fn <- getDataFileName $ "data" ++ daynum ++ ".txt"
  d <- TIO.readFile fn
  return $ day d

createDay :: (Text -> Answer) -> DayNum -> Day
createDay f n = Day { runDay = liftDay n f
                    , numDay = n }

days :: [Day]
days = fmap (uncurry createDay) incompleteDays
  where incompleteDays = [ (day1, "01"), (day2, "02"), (day3, "03")
                         , (day4, "04"), (day5, "05")
                         , (day7, "07"), (day8, "08") ]


