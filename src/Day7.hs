{-# LANGUAGE OverloadedStrings #-}

module Day7 (day7) where

import Data.Text (Text)
import Data.Void
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Part = One | Two

day7 :: Text -> (String, String)
day7 d = (show $ parts One parsed, show $ parts Two parsed)
  where parsed = fromJust $ parseMaybe parser d

parts :: Part -> [(Integer, [Integer])] -> Integer
parts p = sum . fmap (\(t, d) -> if test p (t, d) then t else 0)

test :: Part -> (Integer, [Integer]) -> Bool
test p (t, d) = Set.size (Set.filter (== t) $ findOutcomes p t d) > 0

outcomes :: Part -> Integer -> Integer -> Set Integer
outcomes p a b = case p of
                   One -> Set.fromList [a * b, a + b]
                   Two -> Set.fromList [a * b, a + b, read $ show a ++ show b]

filteredOutcomes :: Part -> Integer -> Integer -> Integer -> Set Integer
filteredOutcomes p t a b = Set.filter (<= t) $ outcomes p a b

findOutcomes :: Part -> Integer -> [Integer] -> Set Integer
findOutcomes p t (a:(b:xs)) = Set.unions
                            $ Set.map (\x -> findOutcomes p t (x:xs)) outs
  where outs = filteredOutcomes p t a b
findOutcomes _  _ l = Set.fromList l

parser :: Parser [(Integer, [Integer])]
parser = many $ do
  t <- decimal
  void $ string ": "
  nums <- many $ do
    n <- decimal
    void $ optional $ char ' '
    return n
  void $ optional eol
  return (t, nums)
