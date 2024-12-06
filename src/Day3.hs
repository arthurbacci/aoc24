{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( day3
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (catMaybes)
import Data.Functor (($>))
import Control.Monad (void)
import Control.Monad.Trans.State (State, modify, get, runState)
import Control.Monad.Trans.Class (lift)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

data Action = AlwaysDo | Do | Don't
  deriving Eq

type Parser = ParsecT Void Text (State Action)

day3 :: Text -> (String, String)
day3 d = (show $ processParsing AlwaysDo d, show $ processParsing Do d)

processParsing :: Action -> Text -> Integer
processParsing a d = case runState (runParserT parser "" d) a of
  (Right x, _) -> multiply x
  (Left _, _) -> error "parsing issue"

multiply :: [(Integer, Integer)] -> Integer
multiply = sum . fmap (uncurry (*))

swapAction :: Action -> Action
swapAction AlwaysDo = AlwaysDo
swapAction Do = Don't
swapAction Don't = Do

parser :: Parser [(Integer, Integer)]
parser = fmap catMaybes $ many
       $ fmap (Just) parseMul <|> parseSwap $> Nothing <|> anySingle $> Nothing

parseMul :: Parser (Integer, Integer)
parseMul = try $ do
  failIfDon't
  void $ string "mul("
  num1 <- decimal
  void $ string ","
  num2 <- decimal
  void $ string ")"
  return (num1, num2)

parseSwap :: Parser ()
parseSwap = do
  a <- lift get
  case a of
    AlwaysDo -> fail ""
    Do       -> void $ string "don't()"
    Don't    -> void $ string "do()"
  lift $ modify swapAction

failIfDon't :: Parser ()
failIfDon't = do
  a <- lift get
  if (a == Don't) then fail ""
  else return ()

