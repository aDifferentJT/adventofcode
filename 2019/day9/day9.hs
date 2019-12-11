
import IntCode (parseProgram, runProgramLists)

import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  print $ runProgramLists program [1]
  print $ runProgramLists program [2]

