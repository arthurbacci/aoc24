module Day2
  ( day2
  ) where


import Data.Text (Text)
import Data.Void
import Text.Megaparsec (Parsec, parseTest)

-- FIXME: remove after parsing is working
import System.IO.Unsafe (unsafePerformIO)

type Parser = Parsec Void Text

day2 :: Text -> (String, String)
day2 d = unsafePerformIO $ do
  parseTest parser d
  return ("", "")

parser :: Parser ()
parser = return ()

