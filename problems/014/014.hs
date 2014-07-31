import Control.Exception.Base (evaluate)
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Clock
import System.Environment (getArgs)
import System.IO

input :: Int
input = 1000000

solution :: Int -> Int
solution n
  = fst
  . maximumBy (comparing snd)
  . assocs
  . collatzLengths
  $ n

collatzLengths :: Int -> Array Int Int
collatzLengths n = a
  where a = listArray (1,n) $ 1:[collatzLength n x | x <- [2..n]]
        collatzLength n x = if x' <= n then a ! x' else 1 + collatzLength n x'
          where x' = if even x then x `div` 2 else 3 * x + 1

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
