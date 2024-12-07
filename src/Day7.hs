{-# LANGUAGE OverloadedStrings #-}

module Day7 (day7) where

import Data.Text (Text)
import Data.Void
import Control.Monad
import Data.Maybe
import Data.List (find)
import Data.List.Index
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
test p (t, d) = isJust $ find (== t) $ listTests p d

listTests :: Part -> [Integer] -> [Integer]
listTests p d = fmap (\x -> doOps d x) $ f $ length d
  where f = case p of
              One -> getOpCombinations
              Two -> get2OpComb

doOps :: [Integer] -> [Op] -> Integer
doOps d o = ifoldl (\a -> \i -> \x -> doOp (o !! i) a x) 0 d

doOp :: Op -> Integer -> Integer -> Integer
doOp Mul = (*)
doOp Add = (+)
doOp Concat = \a -> \b -> read $ show a ++ show b

getOpCombinations :: Int -> [[Op]]
getOpCombinations 0 = []
getOpCombinations 1 = [[Mul], [Add]]
getOpCombinations n = fmap (\a -> [Mul] ++ a) r ++ fmap (\a -> [Add] ++ a) r
  where r = getOpCombinations (n - 1)

get2OpComb :: Int -> [[Op]]
get2OpComb 0 = []
get2OpComb 1 = [[Mul], [Add], [Concat]]
get2OpComb n = fmap (f Mul) r ++ fmap (f Add) r ++ fmap (f Concat) r
  where r = get2OpComb (n - 1)
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
