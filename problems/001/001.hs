import Control.Exception.Base (evaluate)
import System.Clock
import System.Environment (getArgs)
import System.IO

input :: Int
input = 1000

solution :: Int -> Int
solution n
  = sum
  . filter (\x -> x `rem` 3 == 0 || x `rem` 5 == 0)
  $ [1..(n-1)]

run :: (a -> b) -> a -> Int -> IO ()
run fun inp iters
  | iters <= 0 = return ()
  | otherwise = evaluate (fun inp) >> run fun inp (iters-1)

benchmark :: (a -> b) -> a -> Int -> IO Int
benchmark fun inp iters = do
  start <- getTime Monotonic
  run fun inp iters
  end <- getTime Monotonic
  let secs = sec end - sec start
  let nsecs = nsec end - nsec start
  let elapsed = 1000000000 * secs + nsecs
  return elapsed

benchmarkLoop :: IO ()
benchmarkLoop = do
  line <- getLine
  let iters = read line
  time <- benchmark solution input iters
  print time
  hFlush stdout
  benchmarkLoop

main = do
  args <- getArgs
  case args of
    [ "-a" ] -> print $ solution input
    [] -> benchmarkLoop
