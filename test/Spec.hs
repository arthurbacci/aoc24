import Paths_aoc24

import Lib

assertTwo :: [a] -> (a, a)
assertTwo [x, y] = (x, y)
assertTwo _ = error "not two"

dayAnswer :: DayNum -> IO (String, String)
dayAnswer day = do
  fp <- getDataFileName ("answer" ++ day ++ ".txt")
  ans <- readFile fp
  return $ assertTwo $ words ans

daysAndAns :: IO [((String, String), (String, String))]
daysAndAns = days >>= (mapM combineWithAns)
  where
  combineWithAns (d, daynum) = do
    ans <- dayAnswer daynum
    return (d, ans)

main :: IO ()
main = daysAndAns >>= mapM_ checkDay
  where
  checkDay :: ((String, String), (String, String)) -> IO ()
  checkDay (mine, actual) = do
    putStrLn $ (show mine) ++ " x " ++ (show actual)
    if mine /= actual
      then error "mismatch"
      else return ()
