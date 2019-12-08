{-# LANGUAGE TupleSections, ViewPatterns #-}

import Control.Arrow ((***))
import Data.Bool (bool)
import Data.IORef
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe

type Prog = (Int, [Int], [Int], [Int], [Int])

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
getInput (pc, xs, ys, i:is, os) = (i, (pc, xs, ys, is, os))
getInput p = error . show $ p

putOutput :: Int -> Prog -> Prog
putOutput o (pc, xs, ys, is, os) = (pc, xs, ys, is, o:os)

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

runProgram' :: Prog -> ([Int], [Int])
runProgram' (runOpcode -> (True, p)) = runProgram' p
runProgram' (runOpcode -> (False, (_, xs, ys, _, os))) = (reverse xs ++ ys, reverse os)

runProgram :: [Int] -> [Int] -> [Int]
runProgram ys is = snd . runProgram' $ (0, [], ys, is, [])

parseProgram :: String -> [Int]
parseProgram = map read . splitOn ","

{-# NOINLINE program #-}
program :: [Int]
program = unsafePerformIO $ parseProgram <$> readFile "input.txt"

{-# NOINLINE cache #-}
cache :: IORef (Map Int (Int, Int))
cache = unsafePerformIO . newIORef $ Map.empty

maximumWith :: Ord o => (a -> o) -> [a] -> a
maximumWith f = fst . maximumWith' f
  where maximumWith' :: Ord o => (a -> o) -> [a] -> (a, o)
        maximumWith' f [x]    = (x, f x)
        maximumWith' f (x:xs) =
          let fx = f x in
          let (y, fy) = maximumWith' f xs in
          if fx >= fy
          then (x, fx)
          else (y, fy)

bestPhaseForInput :: ([Int] -> Int -> IO Int) -> [Int] -> Int -> IO (Int, Int)
bestPhaseForInput f ps i = maybe bestPhaseForInput' return . Map.lookup i =<< readIORef cache
  where bestPhaseForInput' :: IO (Int, Int)
        bestPhaseForInput' = maximumWith snd <$> mapM (\p -> fmap (p,) . f (filter (/= p) ps) . head . runProgram program $ [p, i]) ps

bestPhaseForInputN :: Int -> [Int] -> Int -> IO (Int, Int)
bestPhaseForInputN 0 _  = return . (0,)
bestPhaseForInputN n ps = bestPhaseForInput ((fmap snd .) . bestPhaseForInputN (n - 1)) ps

main :: IO ()
main = snd <$> bestPhaseForInputN 5 [0..4] 0 >>= print

