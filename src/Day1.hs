module Day1
  ( day1
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (Parsec, parseMaybe, parseTest, many, some)
import Text.Megaparsec.Char (space, hspace1, digitChar)

type Parser = Parsec Void Text

day1 :: Text -> (String, String)
day1 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe parser d


part1 :: ([Integer], [Integer]) -> Integer
part1 (a, b) = sum $ abs <$> (\(x, y) -> x - y) <$> zip (sort a) (sort b)

part2 :: ([Integer], [Integer]) -> Integer
part2 (a, b) = sum $ calcScore <$> a
  where
  calcNumAndOccur x = (NE.head x, toInteger $ length x)
  numsAndOccurs = calcNumAndOccur <$> NE.group (sort b)
  calcScore x = x * (fromMaybe 0 $ lookup x numsAndOccurs)


parser :: Parser ([Integer], [Integer])
parser = unzip <$> many parseLine

parseLine :: Parser (Integer, Integer)
parseLine = do
  x <- parseNum
  hspace1
  y <- parseNum
  space
  return (x, y)

parseNum :: Parser Integer
parseNum = read <$> some digitChar





