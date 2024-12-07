module Day4
  ( day4
  ) where

import Data.Text (Text, unpack, split)
import Data.List (transpose)
import Data.List.Index (imap)

day4 :: Text -> (String, String)
day4 d = (show $ part1 $ parse d, show $ part2 $ parse d)

part1 :: [[Char]] -> Int
part1 d = sum $ fmap countXmas
  $ d ++ transpose d ++ transpose (addSpaces1 d) ++ transpose (addSpaces2 d)

part2 :: [[Char]] -> Int
part2 (a:(b:(c:xs))) = countPart2Row (a:(b:(c:xs))) + part2 (b:(c:xs))
part2 _ = 0

countPart2Row :: [[Char]] -> Int
countPart2Row m
  -- number of columns left
  | (length $ head m) >= 3 = fromEnum (detectPart2Xmas m)
                           + countPart2Row (fmap tail m)
  | otherwise = 0

masDual :: Char -> Char
masDual 'S' = 'M'
masDual 'M' = 'S'
masDual _ = error "no dual for that"

detectPart2Xmas :: [[Char]] -> Bool
detectPart2Xmas m = m !! 1 !! 1 == 'A'
  && (m !! 0 !! 0) `elem` "MS" && (m !! 2 !! 2) == masDual (m !! 0 !! 0)
  && (m !! 0 !! 2) `elem` "MS" && (m !! 2 !! 0) == masDual (m !! 0 !! 2)

countXmas :: [Char] -> Int
countXmas ('X':('M':('A':('S':xs)))) = 1 + countXmas ('S':xs)
countXmas ('S':('A':('M':('X':xs)))) = 1 + countXmas ('X':xs)
countXmas (_:xs) = countXmas xs
countXmas [] = 0

addSpaces1 :: [[Char]] -> [[Char]]
addSpaces1 = imap (\i -> \x -> replicate i ' ' ++ x)

addSpaces2 :: [[Char]] -> [[Char]]
addSpaces2 d = imap (\i -> \x -> replicate (length d - i - 1) ' ' ++ x) d

parse :: Text -> [[Char]]
parse = (fmap unpack) . split (== '\n')

