module Lib
    ( DayNum
    , days
    ) where

import Paths_aoc24
import qualified Data.Text.IO as TIO

import Day1
import Day2

type DayNum = String

days :: IO [((String, String), DayNum)]
days = mapM processDay incompleteDays
  where
  processDay (day, daynum) = do
    fp <- getDataFileName $ "data" ++ daynum ++ ".txt"
    txt <- TIO.readFile fp
    return (day txt, daynum)
  incompleteDays = [(day1, "01"), (day2, "02")]
