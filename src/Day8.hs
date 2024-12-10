module Day8 (day8) where

import Data.Text (Text)
import Data.Void
import Data.Maybe (fromJust)
import Data.Function
import Data.List (groupBy, sortBy, nub)
import Control.Monad (void)
import Control.Lens.Getter (view)
import Text.Megaparsec
import Text.Megaparsec.Char
import Linear.V2

type Parser = Parsec Void Text

type Point = V2 Int

type AerialId = Char

data Aerial = Aerial {  aerialId :: AerialId
                     , aerialPos :: Point } deriving Show

data Antinode = Antinode { originAerialId :: AerialId
                         ,    antinodePos :: Point } deriving (Show, Eq)

data Part = One | Two

day8 :: Text -> (String, String)
day8 d = (show $ part One parsed, show $ part Two parsed)
  where parsed = fromJust $ parseMaybe (parser <* takeRest) d

part :: Part -> ([Aerial], Point) -> Int
part p (a, l) = length $ nub
  $ fmap antinodePos $ concat $ concat
  $ (fmap $ antinodes p l) <$> aerialPairs <$> groupAerials a

isInBounds :: Point -> Point -> Bool
isInBounds l t = xt >= 0 && yt >= 0 && xt <= xl && yt <= yl
  where { xt = view _x t; xl = view _x l
        ; yt = view _y t; yl = view _y l }

antinodes :: Part -> Point -> (Aerial, Aerial) -> [Antinode]
antinodes p l (a1, a2) = genAntinode <$> ((calcApols p l) `on` aerialPos) a1 a2
  where genAntinode p = Antinode { originAerialId = aerialId a1
                                 ,    antinodePos = p }

calcApols :: Part -> Point -> Point -> Point -> [Point]
calcApols p l p1 p2 =
  case p of
    One -> filter (isInBounds l) [calcApol p1 p2 2, calcApol p2 p1 2]
    Two -> concat $ [filterInBounds p1 p2, filterInBounds p2 p1]
  where calcApol a b n = n * b - (n - 1) * a
        filterInBounds a b = takeWhile (isInBounds l)
                           $ (calcApol a b) <$> return <$> [1..]

groupAerials :: [Aerial] -> [[Aerial]]
groupAerials = groupBy ((==) `on` aerialId) . sortBy (compare `on` aerialId)

aerialPairs :: [Aerial] -> [(Aerial, Aerial)]
aerialPairs (x:xs) = fmap (\a -> (x, a)) xs ++ aerialPairs xs
aerialPairs [] = []

parser :: Parser ([Aerial], Point)
parser = do
  aerials <- many (parseAir *> parseAerial <* parseAir)
  p <- getPoint
  return (aerials, p)

parseAerial :: Parser Aerial
parseAerial = do
  aid <- alphaNumChar
  p <- getPoint
  return $ Aerial aid p

getPoint :: Parser Point
getPoint = do
  sourcePos <- getSourcePos
  let x = unPos $ sourceColumn sourcePos
  let y = unPos $ sourceLine sourcePos
  return $ V2 (x - 2) (y - 1)

parseAir :: Parser ()
parseAir = void $ many $ (void $ char '.') <|> (void eol)

