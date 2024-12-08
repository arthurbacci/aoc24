import Criterion.Main
import System.Environment (getArgs)
import Paths_aoc24

import Lib

assertTwo :: [a] -> (a, a)
assertTwo [x, y] = (x, y)
assertTwo _ = error "not two"

actualAnswer :: DayNum -> IO Answer
actualAnswer day = do
  fp <- getDataFileName ("answer" ++ day ++ ".txt")
  ans <- readFile fp
  return $ assertTwo $ words ans

runCheckAndBench :: Day -> IO ()
runCheckAndBench d = do
  mine <- runDay d
  actual <- actualAnswer $ numDay d

  -- Check answer
  putStrLn $ (show mine) ++ " x " ++ (show actual)
  if mine /= actual then error "mismatch"
                    else return ()

  -- Benchmark
  defaultMain [bench (numDay d) $ nfIO $ runDay d]

main :: IO ()
main = do
  args <- getArgs
  let filterDays = if null args then id
                                else filter (\x -> numDay x `elem` args)
  mapM_ runCheckAndBench $ filterDays $ days

