{-# LANGUAGE RecordWildCards #-}

import Control.Arrow (first, second)
import Control.Monad (unless, void)
import Data.Bool (bool)
import Data.Char (ord)
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadPrec
import Text.Read (readPrec)

data Instruction = Instruction { requirement :: Char, consequence :: Char }
  deriving Show

pchar :: Char -> ReadPrec ()
pchar c = do
  c' <- get
  unless (c == c') pfail

pstring :: String -> ReadPrec ()
pstring = void . traverse pchar

instance Read Instruction where
  readPrec = do
    pstring "Step "
    requirement <- get
    pstring " must be finished before step "
    consequence <- get
    pstring " can begin."
    return Instruction{..}

type Dependencies = Map Char String

getDependencies :: [Instruction] -> Dependencies
getDependencies = foldr f Map.empty
  where f :: Instruction -> Dependencies -> Dependencies
        f Instruction{..} =
          Map.insertWith (++) consequence [requirement]
          . Map.insertWith (flip const) requirement ""

performStep :: Dependencies -> Maybe (Char, Dependencies)
performStep m = do
  (x, _) <- Map.lookupMin . Map.filter null $ m
  let m' = Map.map (filter (/= x)) . Map.delete x $ m
  return (x, m')

steps :: Dependencies -> String
steps = unfoldr performStep

timeForStep :: Char -> Int
timeForStep = (+61) . subtract (ord 'A') . ord

type Workers = [(Char, Int)]

assignWorkers :: Workers -> Dependencies -> (Workers, Dependencies)
assignWorkers [] m = ([], m)
assignWorkers (w@(_, 0):ws) m = case Map.lookupMin . Map.filter null $ m of
  Nothing -> (w:ws, m)
  Just (x, _) -> first ((x, timeForStep x):) . assignWorkers ws . Map.delete x $ m
assignWorkers (w:ws) m = first (w:) . assignWorkers ws $ m

advanceTime :: Workers -> Maybe Workers
advanceTime = uncurry (bool Just (const Nothing)) . advanceTime'
  where advanceTime' :: Workers -> (Bool, Workers)
        advanceTime' [] = (True, [])
        advanceTime' (w@(_, 0):ws) = second (w:) . advanceTime' $ ws
        advanceTime' ((x, t):ws) = (False, (x, t - 1) : (snd . advanceTime' $ ws))

completedWorkers :: Workers -> Dependencies -> Dependencies
completedWorkers = flip (foldr f)
  where f :: (Char, Int) -> Dependencies -> Dependencies
        f (x, 0) = Map.map (filter (/= x))
        f _      = id

totalParallelTime :: Int -> Dependencies -> Int
totalParallelTime n = totalParallelTime' (replicate n ('\0', 0))
  where totalParallelTime' :: Workers -> Dependencies -> Int
        totalParallelTime' ws1 m1 = fromMaybe 0 $ do
          let (ws2, m2) = assignWorkers ws1 m1
          ws3 <- advanceTime ws2
          let m4 = completedWorkers ws3 m2
          return $ 1 + totalParallelTime' ws3 m4

{-# NOINLINE dependencies #-}
dependencies :: Dependencies
dependencies = getDependencies . map read . lines . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  print . steps $ dependencies
  print . totalParallelTime 5 $ dependencies

