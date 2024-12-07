{-# LANGUAGE OverloadedStrings #-}

module Day7 (day7) where

import Data.Text (Text)
import Data.Void
import Control.Monad
import Data.Maybe
import Data.List (foldl')
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Part = One | Two

data Op = Mul | Add | Concat
  deriving Show

day7 :: Text -> (String, String)
day7 d = (show $ parts One parsed, show $ parts Two parsed)
  where parsed = fromJust $ parseMaybe parser d

parts :: Part -> [(Integer, [Integer])] -> Integer
parts p = sum . fmap (\(t, d) -> if test p (t, d) then t else 0)

test :: Part -> (Integer, [Integer]) -> Bool
test p (t, d) = any (\o -> testCombinations o t d) $ f $ length d
  where f = case p of
              One -> getAllCombinations [Add, Mul]
              Two -> getAllCombinations [Add, Mul, Concat]

testCombinations :: [Op] -> Integer -> [Integer] -> Bool
testCombinations o t d = (foldl' f 0 $ zip o d) == t
  where f a (op, x) = if a > t then a
                               else doOp op a x

doOp :: Op -> Integer -> Integer -> Integer
doOp Mul = (*)
doOp Add = (+)
doOp Concat = \a -> \b -> read $ show a ++ show b

getAllCombinations :: [Op] -> Int -> [[Op]]
getAllCombinations _ 0 = [[]]
getAllCombinations x n = join $ fmap (\o -> fmap (f o) r) x
  where r = getAllCombinations x (n - 1)
        f o a = [o] ++ a

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
