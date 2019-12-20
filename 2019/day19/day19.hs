{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}

import IntCode (parseProgram, runProgramLists)

import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

scanPoint :: Integer -> Integer -> Integer
scanPoint x y = head . runProgramLists program $ [x, y]

scan :: Integer
scan = sum [ scanPoint x y | x <- [0..49], y <- [0..49] ]

closestSquare :: Integer -> (Integer, Integer)
closestSquare = closestSquare' 0 0
  where closestSquare' :: Integer -> Integer -> Integer -> (Integer, Integer)
        closestSquare' x y w = 
          if scanPoint (x + w) y == 1
          then
            if scanPoint x (y + w) == 1
            then (x, y)
            else closestSquare' (x + 1) y w
          else closestSquare' x (y + 1) w

main :: IO ()
main = do
  print scan
  let (x, y) = closestSquare 99
  print (x * 10000 + y)

