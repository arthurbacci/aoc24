module Day9 (day9) where

import Data.Text (Text)
import Data.Void
import Data.List (singleton, find, foldl', group)
import Data.List.Index (imap)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Control.Monad.Trans.State (State, modify, get, runState)
import Control.Monad.Trans.Class (lift)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = ParsecT Void Text (State ParseState)

data ParseData = Space | File Int
  deriving (Show, Eq)

data ParseState = ParseState { lastParsed :: ParseData
                             ,     nextId :: Int } deriving Show

defaultParseState :: ParseState
defaultParseState = ParseState Space 0

fileNum :: ParseData -> Int
fileNum (File x) = x
fileNum Space = error "no file num for space"

day9 :: Text -> (String, String)
day9 d = case runState (runParserT parser "" d) defaultParseState of
  (Right x, _) -> (show $ part1 x, show $ part2 x)
  (Left _, _)  -> error "couldn't parse"

part1 :: [ParseData] -> Int
part1 d = sum $ imap checksum filled
  where
  numFiles = length $ filter (/= Space) d
  breakPoint = (+ 1) $ fst $ fromJust $ find (\(_, (a, b)) -> a == b)
    $ foldl' foldBreakPoint [(0, (0, numFiles - 1))] d
  toFill = fst $ splitAt breakPoint d
  toUse = filter (/= Space) $ snd $ splitAt breakPoint d
  filled = fst $ foldr foldFill ([], toUse) toFill

part2 :: [ParseData] -> Int
part2 d = sum $ imap checksum $ doMovement Set.empty d
  where
  filesInReverseOrder = reverse $ filter ((/= Space) . head) $ group d
  doMovement _ [] = []
  doMovement processed (File x:xs) = if x `Set.member` processed
                                     then (Space:doMovement processed xs)
                                     else (File x:doMovement processed xs)
  doMovement processed r = case toInsert of
    Just f -> f ++ (doMovement (Set.insert (fileNum $ head f) processed)
                    $ drop (length f) r)
    Nothing -> (Space:(doMovement processed $ tail r))
    where
    numSpaces = length $ head $ group r
    toInsert = find shouldBeInsert filesInReverseOrder
    shouldBeInsert f = (fileNum $ head f) `Set.notMember` processed
      && length f <= numSpaces

checksum :: Int -> ParseData -> Int
checksum i (File x) = i * x
checksum _ Space = 0

foldBreakPoint :: [(Int, (Int, Int))] -> ParseData -> [(Int, (Int, Int))]
foldBreakPoint ((i, (a, b)):r) (File _) = ((i + 1, (a + 1, b)):(i, (a, b)):r)
foldBreakPoint ((i, (a, b)):r) Space    = ((i + 1, (a, b - 1)):(i, (a, b)):r)
foldBreakPoint [] _ = error "shouldn't call without data"

foldFill :: ParseData -> ([ParseData], [ParseData])
         -> ([ParseData], [ParseData])
foldFill (File f) (l, x) = (File f:l, x)
foldFill Space (l, (x:xs)) = (x:l, xs)
foldFill _ _ = error "something wrong when filling"

parser :: Parser [ParseData]
parser = fmap concat $ many $ do
  st <- lift get
  np <- case lastParsed st of
    Space  -> parseFiles $ nextId st
    File _ -> parseSpaces
  lift $ case lastParsed st of
    File _ -> modify $ \x -> ParseState Space $ nextId x
    Space  -> modify $ \x -> ParseState (File $ nextId st)
      $ if null np then nextId x else nextId x + 1
  return np

parseFiles :: Int -> Parser [ParseData]
parseFiles = repeatXTimes . File

parseSpaces :: Parser [ParseData]
parseSpaces = repeatXTimes Space

repeatXTimes :: a -> Parser [a]
repeatXTimes x = do n <- (read . singleton) <$> digitChar
                    return $ take n $ repeat x
