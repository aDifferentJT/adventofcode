{-# LANGUAGE FlexibleInstances, NamedFieldPuns, RecordWildCards, TupleSections, ViewPatterns #-}

module IntCode (ProgIO(..), runProgram, runProgramLists, parseProgram) where

import Control.Arrow ((***))
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

class ProgIO a where
  getInput :: a -> (Integer, a)
  putOutput :: Integer -> a -> a

data Prog a = Prog
  { pc :: Integer
  , relativeBase :: Integer
  , memPrePC :: [Integer]
  , memPostPC :: [Integer]
  , io :: a
  }
  deriving Show

instance ProgIO a => ProgIO (Prog a) where
  getInput p = let (i, io') = getInput . io $ p in (i, p { io = io' })
  putOutput o p = p { io = putOutput o . io $ p }

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

getValue :: (Integer, Integer) -> Prog a -> Integer
getValue (i, 0) Prog{..} = fromMaybe 0 $
  if i >= pc
  then memPostPC !!? (i - pc)
  else memPrePC !!? (pc - i - 1)
getValue (i, 1) _ = i
getValue (i, 2) Prog{..} = getValue (i + relativeBase, 0) Prog{..}

putValue :: (Integer, Integer) -> Integer -> Prog a -> Prog a
putValue (i, 0) v Prog{..} =
  if i >= pc
  then Prog { memPostPC = put 0 (i - pc) v memPostPC, .. }
  else Prog { memPrePC = put 0 (pc - i - 1) v memPrePC, .. }
putValue (i, 2) v Prog{..} = putValue (i + relativeBase, 0) v Prog{..}

incPC :: Integer -> Prog a -> Prog a
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

runOpcode :: ProgIO a => Prog a -> (Bool, Prog a)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (1, s1:s2:d:_)), .. } =
    let res = getValue s1 p + getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (2, s1:s2:d:_)), .. } =
    let res = getValue s1 p * getValue s2 p in
    (True, incPC 4 . putValue d res $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (3, d:_)), .. } =
    (True, incPC 2 . uncurry (putValue d) . getInput $ p)
runOpcode p@Prog{ memPostPC = (getOpcodeAndModes -> (4, s:_)), .. } =
    (True, incPC 2 . putOutput (getValue s p) $ p)
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

runProgram' :: ProgIO a => Prog a -> a
runProgram' (runOpcode -> (True, p)) = runProgram' p
runProgram' (runOpcode -> (False, Prog{io})) = io

runProgram :: ProgIO a => [Integer] -> a -> a
runProgram ys io = runProgram' $ Prog
  { pc = 0
  , relativeBase = 0
  , memPrePC = []
  , memPostPC = ys
  , io = io
  }

instance ProgIO ([Integer], [Integer]) where
  getInput (~(i:is), os) = (i, (is, os))
  putOutput o (is, os) = (is, os ++ [o])

runProgramLists :: [Integer] -> [Integer] -> [Integer]
runProgramLists p = snd . runProgram p . (, [])

parseProgram :: String -> [Integer]
parseProgram = map read . splitOn ","

