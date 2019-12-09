{-# LANGUAGE TupleSections, ViewPatterns #-}

import Control.Arrow ((***))
import Data.List (permutations)
import Data.List.Split (splitOn)

type Prog = (Int, [Int], [Int], [Int], [Maybe Int])

getOpcode :: Int -> Int
getOpcode =
  read . reverse . take 2 . reverse . show

getOpcodeAndModes :: [Int] -> (Int, [(Int, Int)])
getOpcodeAndModes (o:ps) =
  (read . reverse *** zip ps . (++ repeat 0) . map (read . (:[]))) . splitAt 2 . reverse . show $ o

put :: Int -> a -> [a] -> [a]
put 0 x (y:ys) = x : ys
put n x (y:ys) = y : put (n - 1) x ys

getValue :: (Int, Int) -> Prog -> Int
getValue (i, 0) (pc, xs, ys, _, _) =
  if i >= pc
  then ys !! (i - pc)
  else xs !! (pc - i - 1)
getValue (i, 1) _ = i

putValue :: (Int, Int) -> Int -> Prog -> Prog
putValue (i, 0) v (pc, xs, ys, is, os) =
  if i >= pc
  then (pc, xs, put (i - pc) v ys, is, os)
  else (pc, put (pc - i - 1) v xs, ys, is, os)

incPC :: Int -> Prog -> Prog
incPC 0 p = p
incPC n (pc, xs, ys, is, os)
  | n > 0 = incPC (n - 1) (pc + 1, head ys : xs, tail ys, is, os)
  | n < 0 = incPC (n + 1) (pc - 1, tail xs, head xs : ys, is, os)

getInput :: Prog -> (Int, Prog)
getInput (pc, xs, ys, ~(i:is), os) = (i, (pc, xs, ys, is, os)) -- This lazy pattern match is essential for looping between programs

putOutput :: Int -> Prog -> Prog
putOutput o (pc, xs, ys, is, os) = (pc, xs, ys, is, os ++ [Just o])

runOpcode :: Prog -> (Bool, Prog)
runOpcode p@(_, s, getOpcodeAndModes -> (1, s1:s2:d:_), _, _) =
    let res = getValue s1 p + getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@(_, _, getOpcodeAndModes -> (2, s1:s2:d:_), _, _) =
    let res = getValue s1 p * getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@(_, _, getOpcodeAndModes -> (3, d:_), _, _) =
    (True, incPC 2 . uncurry (putValue d) . getInput $ p)
runOpcode p@(_, _, getOpcodeAndModes -> (4, s:_), _, _) =
    (True, incPC 2 . uncurry putOutput . (,p) . getValue s $ p)
runOpcode p@(pc, _, getOpcodeAndModes -> (5, c:d:_), _, _) =
    let jump = if getValue c p /= 0 then getValue d p - pc else 3 in
    (True, incPC jump p)
runOpcode p@(pc, _, getOpcodeAndModes -> (6, c:d:_), _, _) =
    let jump = if getValue c p == 0 then getValue d p - pc else 3 in
    (True, incPC jump p)
runOpcode p@(_, _, getOpcodeAndModes -> (7, s1:s2:d:_), _, _) =
    let res = if getValue s1 p < getValue s2 p then 1 else 0 in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@(_, _, getOpcodeAndModes -> (8, s1:s2:d:_), _, _) =
    let res = if getValue s1 p == getValue s2 p then 1 else 0 in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@(_, _, 99:ys, _, _) = (False, p)
runOpcode p = error . show $ p

justPrefix :: [Maybe a] -> [a]
justPrefix  []          = []
justPrefix (Nothing:xs) = []
justPrefix (Just x :xs) = x : justPrefix xs

runProgram' :: Prog -> ([Int], [Int])
runProgram' (runOpcode -> (True, p)) = runProgram' p
runProgram' (runOpcode -> (False, (_, xs, ys, _, os))) = (reverse xs ++ ys, justPrefix os)

runProgram :: [Int] -> [Int] -> [Int]
runProgram ys is = snd . runProgram' $ (0, [], ys, is, [])

parseProgram :: String -> [Int]
parseProgram = map read . splitOn ","

runWithPhases :: [Int] -> (Int, Int, Int, Int, Int) -> Int
runWithPhases p (pA, pB, pC, pD, pE) = last oE
  where iA = pA : 0 : oE
        iB = pB : oA
        iC = pC : oB
        iD = pD : oC
        iE = pE : oD
        oA = runProgram p iA
        oB = runProgram p iB
        oC = runProgram p iC
        oD = runProgram p iD
        oE = runProgram p iE

main :: IO ()
main = do
  p <- parseProgram <$> readFile "input.txt"
  print . maximum $
    [ runWithPhases p (pA, pB, pC, pD, pE)
      | [pA, pB, pC, pD, pE] <- permutations [5..9]
    ]
  return ()

