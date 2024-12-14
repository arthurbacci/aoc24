{-# LANGUAGE OverloadedStrings #-}

module Day14 (day14) where

import Data.Text (Text)
import Data.Void (Void)
import Data.List (foldl')
import Data.List.Index (modifyAt, ifindIndex)
import Control.Monad (void)
import Control.Lens.Getter (view)
import Data.Maybe (fromJust)
import Text.Megaparsec hiding (initialPos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Linear.V2

type Parser = Parsec Void Text

type Point = V2 Int

data Robot = Robot { initialPos :: Point
                   ,   velocity :: Point } deriving Show

day14 :: Text -> (String, String)
day14 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe parser d

part1 :: [Robot] -> Int
part1 = foldl' (*) 1 . getQuads (101, 103) 100

part2 :: [Robot] -> Int
part2 d = fromJust $ ifindIndex (const $ maybeATree (101, 103))
  $ fmap (\x -> fmap (moveRobot (101, 103) x) d) [0..]

maybeATree :: (Int, Int) -> [Robot] -> Bool
maybeATree b r = (>= 2) $ length $ filter (>= 20)
  $ fmap (\x -> length
                $ filter (\(p, q) -> initialPos q == initialPos p + V2 0 1)
                $ pairs $ filter (isInColumn x) r)
  [0..(snd b) - 1]

pairs :: [a] -> [(a, a)]
pairs (x:xs) = fmap ((,) x) xs ++ fmap (\y -> (y, x)) xs ++ pairs xs
pairs _ = []

isInColumn :: Int -> Robot -> Bool
isInColumn x r = x == (view _x $ initialPos r)

getQuads :: (Int, Int) -> Int -> [Robot] -> [Int]
getQuads b t = foldl' (countRobot b) [0, 0, 0, 0] . fmap (moveRobot b t)

countRobot :: (Int, Int) -> [Int] -> Robot -> [Int]
countRobot (bx, by) q r =
  case ( (bx `quot` 2) `compare` (view _x $ initialPos r)
       , (by `quot` 2) `compare` (view _y $ initialPos r) ) of
    (LT, LT) -> modifyAt 0 (+ 1) q
    (GT, LT) -> modifyAt 1 (+ 1) q
    (GT, GT) -> modifyAt 2 (+ 1) q
    (LT, GT) -> modifyAt 3 (+ 1) q
    _        -> q

moveRobot :: (Int, Int) -> Int -> Robot -> Robot
moveRobot b t r = robotToBounds b
  $ Robot (initialPos r + (return t) * velocity r) (velocity r)

robotToBounds :: (Int, Int) -> Robot -> Robot
robotToBounds b r =
  Robot (V2 (axisToBounds (fst b) rx) (axisToBounds (snd b) ry)) (velocity r)
  where rx = view _x $ initialPos r
        ry = view _y $ initialPos r

axisToBounds :: Int -> Int -> Int
axisToBounds b a
  | a < 0     = axisToBounds b (a + b)
  | a >= b    = a `rem` b
  | otherwise = a

parser :: Parser [Robot]
parser = many $ do
  void $ string "p="
  px <- parseNum
  void $ string ","
  py <- parseNum
  void $ string " v="
  vx <- parseNum
  void $ string ","
  vy <- parseNum
  void $ many eol
  return $ Robot (V2 px py) (V2 vx vy)

parseNum :: Parser Int
parseNum = L.signed space L.decimal

