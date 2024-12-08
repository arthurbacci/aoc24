{-# LANGUAGE OverloadedStrings #-}

module Day7 (day7) where

import Data.Text (Text)
import Data.Void
import Data.Maybe
import Data.List (isSuffixOf)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Part = One | Two
  deriving Eq

data Level = Level { levelIndex :: Int
                   ,  levelNums :: [Integer] }

data Calibration = Calibration {      firstNum :: Integer
                               ,       testNum :: Integer
                               , intermediates :: [Integer] }

day7 :: Text -> (String, String)
day7 d = (show $ part One parsed, show $ part Two parsed)
  where parsed = fromJust $ parseMaybe parser d

part :: Part -> [Calibration] -> Integer
part p = sum . (fmap testNum) . (filter $ test p)

test :: Part -> Calibration -> Bool
test p c = firstNum c `elem` (levelNums $ firstLevel p c)

lastLevel :: Part -> Calibration -> Level
lastLevel p c = until (\l -> levelIndex l == length (intermediates c))
                (nextLevel p $ intermediates c)
                (Level 0 [firstNum c])

firstLevel :: Part -> Calibration -> Level
firstLevel p c = until (\l -> levelIndex l == 0)
                 (previousLevel p $ intermediates c)
                 (Level (length $ intermediates c) [testNum c])

nextLevel :: Part -> [Integer] -> Level -> Level
nextLevel p is l =
  Level { levelIndex = levelIndex l + 1
        ,  levelNums = concat $ (forwardAlts p n) <$> levelNums l }
  where n = is !! (levelIndex l)

-- TODO: dry
previousLevel :: Part -> [Integer] -> Level -> Level
previousLevel p is l =
  Level { levelIndex = levelIndex l - 1
        ,  levelNums = concat $ (backwardAlts p n) <$> levelNums l }
  where n = is !! (levelIndex l - 1)

forwardAlts :: Part -> Integer -> Integer -> [Integer]
forwardAlts p n x = case p of One -> [x + n, x * n]
                              Two -> [x + n, x * n, read $ show x ++ show n]

backwardAlts :: Part -> Integer -> Integer -> [Integer]
backwardAlts p n x = maybeToList (undoAdd n x) ++ maybeToList (undoMul n x)
  ++ if p == Two then maybeToList (read <$> undoConcat (show n) (show x))
                 else []

undoAdd :: Integer -> Integer -> Maybe Integer
undoAdd n x = if x >= n then Just $ x - n else Nothing

undoMul :: Integer -> Integer -> Maybe Integer
undoMul n x = case x `divMod` n of (r, 0) -> Just r
                                   _      -> Nothing

undoConcat :: String -> String -> Maybe String
undoConcat n x
  | n `isSuffixOf` x && x /= n = Just $ take (length x - length n) x
  | otherwise = Nothing

parser :: Parser [Calibration]
parser = many $ do
  t <- decimal
  void $ string ": "
  nums <- some $ do
    n <- decimal
    void $ optional $ char ' '
    return n
  void $ optional eol
  return $ Calibration (head nums) t (tail nums)
