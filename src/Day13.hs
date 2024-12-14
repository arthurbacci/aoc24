{-# LANGUAGE OverloadedStrings #-}

module Day13 (day13) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (fromJust, catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

type Machine = ( (Integer, Integer, Integer)
               , (Integer, Integer, Integer) )

day13 :: Text -> (String, String)
day13 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe parser d

part1 :: [Machine] -> Integer
part1 d = sum $ catMaybes $ fmap solveMachine d

part2 :: [Machine] -> Integer
part2 = part1
  . fmap (\( (ax, bx, px)
           , (ay, by, py) ) -> ( (ax, bx, px + 10000000000000)
                               , (ay, by, py + 10000000000000) ))

solveMachine :: Machine -> Maybe Integer
solveMachine ( (ax, bx, px)
             , (ay, by, py) ) =
  if a * ax + b * bx == px && a * ay + b * by == py
  then Just $ a * 3 + b
  else Nothing
  where a = (py * bx - px * by) `quot` (ay * bx - ax * by)
        b = (py * ax - px * ay) `quot` (ax * by - ay * bx)

parser :: Parser [Machine]
parser = many $ do
  void $ string "Button A: X+"
  ax <- decimal
  void $ string ", Y+"
  ay <- decimal
  void eol
  void $ string "Button B: X+"
  bx <- decimal
  void $ string ", Y+"
  by <- decimal
  void eol
  void $ string "Prize: X="
  px <- decimal
  void $ string ", Y="
  py <- decimal
  void $ many eol
  return ( (ax, bx, px)
         , (ay, by, py) )

