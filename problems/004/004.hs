import Control.Exception.Base (evaluate)
import Data.Maybe (mapMaybe)
import System.Clock
import System.Environment (getArgs)
import System.IO

input :: Int
input = 1000

isPalindrome :: Int -> Bool
isPalindrome n = s == reverse s
  where s = show n

solution :: Int -> Int
solution n
  = head
  . filter isPalindrome
  . mapMaybe fst
  $ iterate next (Nothing, products)
    where products = [[x * y | y <- [x,x-1..start]] | x <- [end,end-1..start]]
          start = 100
          end = n - 1

next :: (Maybe Int, [[Int]]) -> (Maybe Int, [[Int]])
next (_, xs@(xh:xt):ys@(yh:_):zs)
  | xh > yh = (Just xh, xt:ys:zs)
  | otherwise = (h, xs:zs')
    where (h, zs') = next (Nothing, ys:zs)

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
