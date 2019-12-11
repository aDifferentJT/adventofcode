
import IntCode (parseProgram, runProgramLists)

import Data.List (permutations)
import System.IO.Unsafe (unsafePerformIO)

runWithPhases :: [Integer] -> (Integer, Integer, Integer, Integer, Integer) -> Integer
runWithPhases p (pA, pB, pC, pD, pE) = last oE
  where iA = pA : 0 : oE
        iB = pB : oA
        iC = pC : oB
        iD = pD : oC
        iE = pE : oD
        oA = runProgramLists p iA
        oB = runProgramLists p iB
        oC = runProgramLists p iC
        oD = runProgramLists p iD
        oE = runProgramLists p iE

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = print . maximum $
  [ runWithPhases program (pA, pB, pC, pD, pE)
    | [pA, pB, pC, pD, pE] <- permutations [5..9]
  ]

