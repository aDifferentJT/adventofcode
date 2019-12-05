{-# LANGUAGE ViewPatterns #-}

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

put :: Int -> a -> [a] -> [a]
put 0 x (y:ys) = x : ys
put n x (y:ys) = y : put (n - 1) x ys

getValue :: Int -> (Int, [Int], [Int]) -> Int
getValue i (pc, xs, ys) =
  if i >= pc
  then ys !! (i - pc)
  else xs !! (pc - i - 1)

putValue :: Int -> Int -> (Int, [Int], [Int]) -> (Int, [Int], [Int])
putValue i v (pc, xs, ys) =
  if i >= pc
  then (pc, xs, put (i - pc) v ys)
  else (pc, put (pc - i - 1) v xs, ys)

runOpcode :: (Int, [Int], [Int]) -> (Bool, (Int, [Int], [Int]))
runOpcode p@(_, s, 1:s1:s2:d:ys) =
    let res = getValue s1 p + getValue s2 p in
    (True, putValue d res p)
runOpcode p@(_, _, 2:s1:s2:d:ys) =
    let res = getValue s1 p * getValue s2 p in
    (True, putValue d res p)
runOpcode p@(_, _, 99:ys) = (False, p)

incPC :: (Int, [Int], [Int]) -> (Int, [Int], [Int])
incPC (pc, xs, y1:y2:y3:y4:ys) = (pc + 4, y4:y3:y2:y1:xs, ys)

runProgram' :: (Int, [Int], [Int]) -> [Int]
runProgram' (runOpcode -> (True, p')) = runProgram' . incPC $ p'
runProgram' (runOpcode -> (False, (_, xs, ys))) = reverse xs ++ ys

runProgram :: [Int] -> [Int]
runProgram ys = runProgram' (0, [], ys)

parseProgram :: String -> [Int]
parseProgram = map read . splitOn ","

run :: [Int] -> Int -> Int -> Int
run p1 noun verb =
  let p2 = put 1 noun . put 2 verb $ p1 in
  let p3 = runProgram p2 in
  let (res:_) = p3 in
  res

findNounVerb :: [Int] -> Int -> (Int, Int)
findNounVerb p1 res =
    let (n, v) = f 0 in
    (n, v)
  where g :: Int -> Int -> Maybe (Int, Int)
        g n (-1) = Nothing
        g n v    =
          if run p1 n v == res
          then Just (n, v)
          else g n (v - 1)
        f :: Int -> (Int, Int)
        f n = fromMaybe (f (n + 1)) (g n n)

main :: IO ()
main = do
  f <- readFile "input.txt"
  let p1 = parseProgram f
  print (run p1 12 02)
  let (noun, verb) = findNounVerb p1 19690720
  print (100 * noun + verb)

