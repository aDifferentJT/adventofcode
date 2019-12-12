{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

import Control.Monad (unless, void)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec

data MoonOrdinate = MoonOrdinate
  { p :: Int
  , v :: Int
  }
  deriving (Eq, Ord, Show)

data Moon = Moon
  { x :: MoonOrdinate
  , y :: MoonOrdinate
  , z :: MoonOrdinate
  }

pchar :: Char -> ReadPrec ()
pchar c = do
  c' <- get
  unless (c == c') pfail

pstring :: String -> ReadPrec ()
pstring = void . traverse pchar

instance Read Moon where
  readPrec = do
    pstring "<x="
    x <- readPrec
    pstring ", y="
    y <- readPrec
    pstring ", z="
    z <- readPrec
    pstring ">"
    return Moon
      { x = MoonOrdinate { p = x, v = 0 }
      , y = MoonOrdinate { p = y, v = 0 }
      , z = MoonOrdinate { p = z, v = 0 }
      }

applyGravity :: [MoonOrdinate] -> [MoonOrdinate]
applyGravity ms = map (f ms) ms
  where f :: [MoonOrdinate] -> MoonOrdinate -> MoonOrdinate
        f ms m = m { v = v m + (sum . map (signum . subtract (p m) . p) $ ms) }

applyVelocity :: [MoonOrdinate] -> [MoonOrdinate]
applyVelocity = map f
  where f :: MoonOrdinate -> MoonOrdinate
        f MoonOrdinate{p, v} = MoonOrdinate { p = p + v, v }

applyStep :: [MoonOrdinate] -> [MoonOrdinate]
applyStep = applyVelocity . applyGravity

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n = foldr (.) id (replicate n f)

applySteps :: Int -> [MoonOrdinate] -> [MoonOrdinate]
applySteps = applyNTimes applyStep

stepsToRepeatOrdinate :: [MoonOrdinate] -> Integer
stepsToRepeatOrdinate = stepsToRepeat' Set.empty
  where stepsToRepeat' :: Set [MoonOrdinate] -> [MoonOrdinate] -> Integer
        stepsToRepeat' prev ms
          | Set.member ms prev = 0
          | otherwise = 1 + stepsToRepeat' (Set.insert ms prev) (applyStep ms)

stepsToRepeat :: [Moon] -> Integer
stepsToRepeat ms =
  let stepsX = stepsToRepeatOrdinate . map x $ ms in
  let stepsY = stepsToRepeatOrdinate . map y $ ms in
  let stepsZ = stepsToRepeatOrdinate . map z $ ms in
  foldr lcm 1 [stepsX, stepsY, stepsZ]

{-# NOINLINE startConditions #-}
startConditions :: [Moon]
startConditions = map read . lines . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = print . stepsToRepeat $ startConditions

