{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')
import System.IO.Unsafe (unsafePerformIO)

ones :: Int -> Int
ones = flip mod 10 . abs

pattern' :: Int -> [Int]
pattern' n = tail . cycle . concatMap (replicate n) $ [0, 1, 0, -1]

calcElement :: Int -> [Int] -> Int -> Int
calcElement !accum [] _ = accum
calcElement !accum !xs n =
  let xs2 = drop n xs in
  let (ys, xs3) = splitAt n xs2 in
  let xs4 = drop n xs3 in
  let (zs, xs5) = splitAt n xs4 in
  calcElement (sum ys + sum zs + accum) xs5 n

phase :: Int -> [Int] -> [Int]
phase n is = map (ones . calcElement 0 (0:is)) [1..n]

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n = foldl' (.) id (replicate n f)

{-# NOINLINE input #-}
input :: [Int]
input = map (read . (:[])) . filter (/= '\n') . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  let inputLength = length input
  putStrLn . concatMap show . take 8 . applyNTimes (phase inputLength) 100 $ input
  let signalLength = inputLength * 10000
  let signal = concat . replicate 10000 $ input
  let offset = foldl' ((+) . (*10)) 0 . take 7 $ signal
  --let signalOutput = applyNTimes (phase signalLength) 2 signal
  --let message = take 8 . drop offset $ signalOutput
  --putStrLn . concatMap show $ message
  let signalOutput1 = phase signalLength signal
  mapM_ print signalOutput1
  let signalOutput2 = phase signalLength signalOutput1
  seq signalOutput2 . print . length $ signalOutput2

