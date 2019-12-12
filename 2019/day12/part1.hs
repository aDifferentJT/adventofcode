{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

import Control.Monad (unless, void)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec

type Vec3 = (Int, Int, Int)

instance Num Vec3 where
  (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  (*) = undefined
  abs = undefined
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger 0 = (0, 0, 0)
  fromInteger _ = undefined
  negate (x, y, z) = (-x, -y, -z)

data Moon = Moon
  { p :: Vec3
  , v :: Vec3
  }
  deriving (Eq, Ord, Show)

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
    return Moon { p = (x, y, z), v = 0 }

applyGravity :: [Moon] -> [Moon]
applyGravity ms = map (f ms) ms
  where f :: [Moon] -> Moon -> Moon
        f ms m = m { v = v m + (sum . map (signum . subtract (p m) . p) $ ms) }

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map f
  where f :: Moon -> Moon
        f Moon{p, v} = Moon { p = p + v, v }

applyStep :: [Moon] -> [Moon]
applyStep = applyVelocity . applyGravity

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes f n = foldr (.) id (replicate n f)

applySteps :: Int -> [Moon] -> [Moon]
applySteps = applyNTimes applyStep

moonEnergy :: Moon -> Int
moonEnergy Moon{ p = (x, y, z), v = (vx, vy, vz) } = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map moonEnergy

{-# NOINLINE startConditions #-}
startConditions :: [Moon]
startConditions = map read . lines . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = print . totalEnergy . applySteps 1000 $ startConditions

