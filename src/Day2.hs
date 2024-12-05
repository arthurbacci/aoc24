module Day2
  ( day2
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (group, maximumBy, findIndex, sort)
import Data.Function (on)
import Control.Monad (void)
import Text.Megaparsec (Parsec, parseMaybe, many, some)
import Text.Megaparsec.Char (hspace, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

day2 :: Text -> (String, String)
day2 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe parser d

part1 :: [[Integer]] -> Int
part1 = length . filter id . (fmap $ \x -> checkRule1 x && checkRule2 x)

part2 :: [[Integer]] -> Int
part2 = length . filter id . (fmap checkSubRules)

checkSubRules :: [Integer] -> Bool
checkSubRules l =
  let subs = [length l] ++ subRule1 l ++ subRule2 l
      replaceds = (`removed` l) <$> subs
  in any (\x -> checkRule1 x && checkRule2 x) replaceds

checkRule1 :: [Integer] -> Bool
checkRule1 = (== 1) . length . group . (fmap $ uncurry compare) . pairs

subRule1 :: [Integer] -> [Int]
subRule1 l = fromMaybe [] $ (fmap $ \x -> [x, x + 1])
           $ findIndex (/= getMainOrdering comparisons) comparisons
  where comparisons = (uncurry compare) <$> pairs l

checkRule2 :: [Integer] -> Bool
checkRule2 = all (\(x, y) -> abs (x - y) <= 3 && x /= y) . pairs

subRule2 :: [Integer] -> [Int]
subRule2 l = fromMaybe [] $ (fmap $ \x -> [x, x + 1])
           $ findIndex (\(a, b) -> abs (a - b) > 3 || a == b) $ pairs l

getMainOrdering :: [Ordering] -> Ordering
getMainOrdering = head . (maximumBy (compare `on` length)) . group . sort

removed :: Int -> [a] -> [a]
removed 0 (_:xs) = xs
removed i l = (take i l) ++ (drop (i + 1) l)

pairs :: [a] -> [(a, a)]
pairs (x:(y:ys)) = [(x, y)] ++ pairs (y:ys)
pairs _ = []

parser :: Parser [[Integer]]
parser = many $ do
  ns <- some $ do
    n <- decimal
    hspace
    return n
  void $ many eol
  return ns

