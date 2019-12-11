{-# LANGUAGE RecordWildCards, ViewPatterns #-}

import Control.Arrow ((***))
import Data.List (permutations, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

data Prog = Prog
  { pc :: Integer
  , relativeBase :: Integer
  , memPrePC :: [Integer]
  , memPostPC :: [Integer]
  , input :: [Integer]
  , output :: [Integer]
  }
  deriving Show

getOpcode :: Integer -> Integer
getOpcode =
  read . reverse . take 2 . reverse . show

getOpcodeAndModes :: [Integer] -> (Integer, [(Integer, Integer)])
getOpcodeAndModes (o:ps) =
  (read . reverse *** zip ps . (++ repeat 0) . map (read . (:[]))) . splitAt 2 . reverse . show $ o

(!!?) :: [a] -> Integer -> Maybe a
[]     !!? _ = Nothing
(x:xs) !!? n
  | n == 0 = Just x
  | n <  0 = Nothing
  | n >  0 = xs !!? (n-1)

put :: a -> Integer -> a -> [a] -> [a]
put _ 0 x (maybe [] snd . uncons -> ys) = x : ys
put d n x (fromMaybe (d, []) . uncons -> (y,ys)) = y : put d (n - 1) x ys

getValue :: (Integer, Integer) -> Prog -> Integer
getValue (i, 0) Prog{..} = fromMaybe 0 $
  if i >= pc
  then memPostPC !!? (i - pc)
  else memPrePC !!? (pc - i - 1)
getValue (i, 1) _ = i
getValue (i, 2) Prog{..} = getValue (i + relativeBase, 0) Prog{..}

putValue :: (Integer, Integer) -> Integer -> Prog -> Prog
putValue (i, 0) v Prog{..} =
  if i >= pc
  then Prog { memPostPC = put 0 (i - pc) v memPostPC, .. }
  else Prog { memPrePC = put 0 (pc - i - 1) v memPrePC, .. }
putValue (i, 2) v Prog{..} = putValue (i + relativeBase, 0) v Prog{..}

incPC :: Integer -> Prog -> Prog
incPC 0 p = p
incPC n Prog{..}
  | n > 0 =
    incPC (n - 1)
      $ Prog
        { pc = pc + 1
        , memPrePC = head memPostPC : memPrePC
        , memPostPC = tail memPostPC
        , ..
        }
  | n < 0 =
    incPC (n + 1)
      $ Prog
        { pc = pc - 1
        , memPrePC = tail memPrePC
        , memPostPC = head memPrePC : memPostPC
        , ..
        }

getInput :: Prog -> (Integer, Prog)
getInput Prog{ input = ~(i:is), .. } = (i, Prog { input = is, .. }) -- This lazy pattern match is essential for looping between programs

putOutput :: Prog -> Integer -> Prog
putOutput Prog{..} o = Prog{ output = output ++ [o], .. }

runOpcode :: Prog -> (Bool, Prog)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (1, s1:s2:d:_)), .. } =
    let res = getValue s1 p + getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (2, s1:s2:d:_)), .. } =
    let res = getValue s1 p * getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (3, d:_)), .. } =
    (True, incPC 2 . uncurry (putValue d) . getInput $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (4, s:_)), .. } =
    (True, incPC 2 . putOutput p . getValue s $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (5, c:d:_)), .. } =
    let jump = if getValue c p /= 0 then getValue d p - pc else 3 in
    (True, incPC jump p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (6, c:d:_)), .. } =
    let jump = if getValue c p == 0 then getValue d p - pc else 3 in
    (True, incPC jump p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (7, s1:s2:d:_)), .. } =
    let res = if getValue s1 p < getValue s2 p then 1 else 0 in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (8, s1:s2:d:_)), .. } =
    let res = if getValue s1 p == getValue s2 p then 1 else 0 in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (9, x:_)), .. } =
    (True, incPC 2 $ p { relativeBase = getValue x p + relativeBase })
runOpcode p@Prog{ memPostPC = 99:ys, .. } = (False, p)
runOpcode p = error . show $ p

runProgram' :: Prog -> [Integer]
runProgram' (runOpcode -> (True, p)) = runProgram' p
runProgram' (runOpcode -> (False, Prog{..})) = output

runProgram :: [Integer] -> [Integer] -> [Integer]
runProgram ys is = runProgram' $ Prog
  { pc = 0
  , relativeBase = 0
  , memPrePC = []
  , memPostPC = ys
  , input = is
  , output = []
  }

parseProgram :: String -> [Integer]
parseProgram = map read . splitOn ","

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  print $ runProgram program [1]
  print $ runProgram program [2]
  return ()

