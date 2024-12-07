module Day5 (day5) where

import Data.Text (Text)
import Data.Void
import Data.List (sortBy, elemIndex)
import Data.Maybe
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

day5 :: Text -> (String, String)
day5 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe (parser <* takeRest) d

part1 :: ([(Integer, Integer)], [[Integer]]) -> Integer
part1 (rules, updates) = sum $ fmap middleElem $ filter (rules `allows`) updates

part2 :: ([(Integer, Integer)], [[Integer]]) -> Integer
part2 (rules, updates) = sum $ fmap (middleElem . (rules `useToSort`))
  $ filter (not . (rules `allows`)) updates

allows :: [(Integer, Integer)] -> [Integer] -> Bool
allows rules update = all (checkRule update) rules

checkRule :: [Integer] -> (Integer, Integer) -> Bool
checkRule update (a, b) = case (a `elemIndex` update, b `elemIndex` update) of
  (Just x, Just y) -> y > x
  _ -> True

middleElem :: [a] -> a
middleElem l = l !! (length l `div` 2)

useToSort :: [(Integer, Integer)] -> [Integer] -> [Integer]
useToSort l = sortBy f
  where f a b = case ((a, b) `elem` l, (b, a) `elem` l) of
                  (True, False) -> LT
                  (False, True) -> GT
                  _ -> EQ

parser :: Parser ([(Integer, Integer)], [[Integer]])
parser = do
  rules <- many parseRule
  void $ eol
  updates <- many parseUpdate
  return (rules, updates)

parseRule :: Parser (Integer, Integer)
parseRule = do
  n1 <- decimal
  void $ char '|'
  n2 <- decimal
  void $ optional eol
  return (n1, n2)

parseUpdate :: Parser [Integer]
parseUpdate = (some $ decimal <* optional (char ',')) <* optional eol


