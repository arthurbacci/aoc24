module Day12 (day12) where

import Data.Text (Text)
import Data.Void
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.HashSet (HashSet)
import Data.List (groupBy, sortBy, foldl', partition)
import qualified Data.HashSet as HashSet
import Control.Monad (void)
import Control.Lens.Getter (view)
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type Pt = V2 Int

data Dir = L | R | U | D deriving (Eq, Ord, Show)

day12 :: Text -> (String, String)
day12 d = (show $ part1 parsed, show $ part2 parsed)
  where parsed = fromJust $ parseMaybe (parser <* takeRest) d

part1 :: [(Char, HashSet Pt)] -> Int
part1 = sum . fmap (\(_, s) -> calcArea s * calcPerimeter s)

part2 :: [(Char, HashSet Pt)] -> Int
part2 = sum . fmap (\(_, s) -> calcArea s * calcSides s)

calcArea :: HashSet Pt -> Int
calcArea = HashSet.size

calcPerimeter :: HashSet Pt -> Int
calcPerimeter s = sum $ fmap (length . calcBorder s) $ HashSet.toList s

calcSides :: HashSet Pt -> Int
calcSides s = length $ joinAreas
  $ concat $ fmap (calcDirBorder s) $ HashSet.toList s

calcDirBorder :: HashSet Pt -> Pt -> [(Dir, Pt)]
calcDirBorder s p = filter (not . (`HashSet.member` s) . snd) $ dirNeighbours p

calcBorder :: HashSet Pt -> Pt -> [Pt]
calcBorder s p = filter (not . (`HashSet.member` s)) $ neighbours p

neighbours :: Pt -> [Pt]
neighbours p = [V2 (x + 1) y, V2 (x - 1) y, V2 x (y + 1), V2 x (y - 1)]
  where x = view _x p
        y = view _y p

dirNeighbours :: Pt -> [(Dir, Pt)]
dirNeighbours p = [(R, n !! 0), (L, n !! 1), (D, n !! 2), (U, n !! 3)]
  where n = neighbours p

parser :: Parser [(Char, HashSet Pt)]
parser = (fmap joinAreas) $ many $ do
  c <- alphaNumChar
  pt <- getPoint
  void $ optional eol
  return (c, pt)

-- makes an associative list for each character or direction
joinAreas :: (Ord a) => [(a, Pt)] -> [(a, HashSet Pt)]
joinAreas d = concat $ fmap (\(a, pts) -> (\x -> (a, x)) <$> segregateArea pts)
  $ (fmap groupToAssoc) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) d

segregateArea :: [Pt] -> [HashSet Pt]
segregateArea = foldl' (\a -> \x -> x `tryAddTo` a) []

tryAddTo :: Pt -> [HashSet Pt] -> [HashSet Pt]
tryAddTo p s = (p `HashSet.insert` HashSet.unions containing):notContaining
  where (containing, notContaining) = partition (p `isTouching`) s

isTouching :: Pt -> HashSet Pt -> Bool
isTouching p s = any (`HashSet.member` s) $ neighbours p

groupToAssoc :: [(a, Pt)] -> (a, [Pt])
groupToAssoc ((c, p1):xs) = (c, p1:(fmap snd xs))
groupToAssoc _ = error "impossible empty group"

getPoint :: Parser Pt
getPoint = do
  sourcePos <- getSourcePos
  let x = unPos $ sourceColumn sourcePos
  let y = unPos $ sourceLine sourcePos
  return $ V2 (x - 2) (y - 1)
