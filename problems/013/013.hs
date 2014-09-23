import Control.Exception.Base (evaluate)
import System.Clock
import System.Environment (getArgs)
import System.IO

solution :: String -> Int
solution string
  = read
  . take 10
  . show
  . sum
  . map read
  . lines
  $ string

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

benchmarkLoop :: String -> IO ()
benchmarkLoop string = do
  line <- getLine
  let iters = read line
  time <- benchmark solution string iters
  print time
  hFlush stdout
  benchmarkLoop string

main = do
  args <- getArgs
  string <- readFile "013.txt"
  case args of
    [ "-a" ] -> print $ solution string
    [] -> benchmarkLoop string
