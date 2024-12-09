module Day8 (day8) where

import Data.Text (Text)
import Data.Void
import Data.Maybe (fromJust)
import Data.Function
import Data.List (groupBy, sortBy, nub)
import Data.Ix
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Point = Point { pX :: Int
                   , pY :: Int }
  deriving (Show, Eq)

type AerialId = Char

data Aerial = Aerial {  aerialId :: AerialId
                     , aerialPos :: Point }
  deriving Show

data Antinode = Antinode { originAerialId :: AerialId
                         ,    antinodePos :: Point }
  deriving (Show, Eq)

day8 :: Text -> (String, String)
day8 d = (show $ part1 parsed, show $ snd parsed)
  where parsed = fromJust $ parseMaybe (parser <* takeRest) d

part1 :: ([Aerial], Point) -> Int
part1 (a, p) = length $ removeOutOfBounds p $ nub
  $ fmap antinodePos $ concat $ concat
  $ (fmap antinodes) <$> aerialPairs <$> groupAerials a

removeOutOfBounds :: Point -> [Point] -> [Point]
removeOutOfBounds p = filter
  $ \t -> pX t >= 0 && pY t >= 0 && pX t <= pX p && pY t <= pY p

antinodes :: (Aerial, Aerial) -> [Antinode]
antinodes (a1, a2) = fmap genAntinode $ calcApols (aerialPos a1) (aerialPos a2)
  where
  genAntinode p = Antinode { originAerialId = aerialId a1
                           ,    antinodePos = p }
  calcApols p1 p2 = concat $ fmap (calcApol p1 p2) $ range (1, 100)
  calcApol p1 p2 n = [ Point (n * pX p2 - (n - 1) * pX p1) (n * pY p2 - (n - 1) * pY p1)
                     , Point (n * pX p1 - (n - 1) * pX p2) (n * pY p1 - (n - 1) * pY p2) ]


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
  return $ Point (x - 2) (y - 1)

parseAir :: Parser ()
parseAir = void $ many $ (void $ char '.') <|> (void eol)

