{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( day3
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Control.Monad (void)
import Text.Megaparsec (Parsec, parseTest, anySingle, many, (<|>), try, parseMaybe, takeRest)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

-- FIXME: remove after parsing is working
import System.IO.Unsafe (unsafePerformIO)

type Parser = Parsec Void Text

day3 :: Text -> (String, String)
--day3 d = unsafePerformIO $ do
--  parseTest parser d
--  return ("", "")
day3 d = (show $ part1 parsed, "")
  where parsed = fromJust $ parseMaybe parser d

part1 :: [(Integer, Integer)] -> Integer
part1 = sum . fmap (uncurry (*))

parser :: Parser [(Integer, Integer)]
parser = many findMul <* takeRest

findMul :: Parser (Integer, Integer)
findMul = parseMul <|> (try $ anySingle >> findMul)

parseMul :: Parser (Integer, Integer)
parseMul = try $ do
  void $ string "mul("
  num1 <- decimal
  void $ string ","
  num2 <- decimal
  void $ string ")"
  return (num1, num2)
