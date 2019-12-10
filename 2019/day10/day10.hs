{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Control.Arrow ((***), (&&&))
import Data.Array
import Data.Foldable (asum)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import System.IO.Unsafe (unsafePerformIO)

data Map = Map { m :: Array Int (Array Int Bool), s :: Size }
  deriving Show
data Coord = Coord { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)
data Size = Size { left :: Int, right :: Int, top :: Int, bottom :: Int }
  deriving Show
data Vector = Vector { i :: Int, j :: Int }
  deriving Show

arrayZeroInt :: [e] -> Array Int e
arrayZeroInt xs = listArray (0, length xs - 1) xs

parseMap :: String -> Map
parseMap = makeMap . arrayZeroInt . map (arrayZeroInt . mapMaybe f) . lines
  where f :: Char -> Maybe Bool
        f '.' = Just False
        f '#' = Just True
        f  _  = Nothing

makeMap :: Array Int (Array Int Bool) -> Map
makeMap m = Map m $ uncurry (uncurry Size (bounds $ m ! 0)) (bounds m)

mapLookup :: Map -> Coord -> Bool
mapLookup Map{m} Coord{..} = m ! y ! x

isCoprime :: Integral a => a -> a -> Bool
isCoprime = ((== 1) .) . gcd

visibleVectors :: Size -> Coord -> [Vector]
visibleVectors Size{..} Coord{..} = [ Vector{i, j} | i <- [left - x .. right - x], j <- [top - y .. bottom - y], isCoprime i j ]

scale :: Vector -> Int -> Vector
scale = (uncurry Vector .) . uncurry (&&&) . ((*) *** (*)) . (i &&& j)

offset :: Coord -> Vector -> Coord
offset Coord{..} Vector{..} = Coord (x + i) (y + j)

withinSize :: Size -> Coord -> Bool
withinSize Size{..} Coord{..} = and
  [ x >= left
  , x <= right
  , y >= top
  , y <= bottom
  ]

isAsteroidOnVector :: Map -> Coord -> Vector -> Maybe Coord
isAsteroidOnVector m@Map{s} c v = asum . map (\c -> if mapLookup m c then Just c else Nothing) . takeWhile (withinSize s) . map (offset c . scale v) $ [1..]

data Quadrant = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Ord)

quadrant :: Vector -> Quadrant
quadrant Vector{..}
  | i == 0 && j <  0 = N
  | i >  0 && j <  0 = NE
  | i >  0 && j == 0 = E
  | i >  0 && j >  0 = SE
  | i == 0 && j >  0 = S
  | i <  0 && j >  0 = SW
  | i <  0 && j == 0 = W
  | i <  0 && j <  0 = NW

compDestroyed :: Vector -> Vector -> Ordering
compDestroyed v1@(Vector i1 j1) v2@(Vector i2 j2) =
  case compare (quadrant v1) (quadrant v2) of
    EQ -> compare (fromIntegral j1 / fromIntegral i1) (fromIntegral j2 / fromIntegral i2)
    o -> o

sortByDestroyed :: [Vector] -> [Vector]
sortByDestroyed = sortBy compDestroyed

visibleAsteroidsFrom :: Map -> Coord -> [Coord]
visibleAsteroidsFrom m@Map{s} c = mapMaybe (isAsteroidOnVector m c) . sortByDestroyed . visibleVectors s $ c

allAsteroids :: Map -> [Coord]
allAsteroids m@Map{s=Size{..}} = [ c | x <- [left..right], y <- [left..right], let c = Coord{x, y}, mapLookup m c ]

mostVisible :: Map -> (Int, Coord)
mostVisible m = maximum . map ((length &&& (!! 199)) . visibleAsteroidsFrom m) . allAsteroids $ m

{-# NOINLINE asteroidMap #-}
asteroidMap :: Map
asteroidMap = parseMap . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = print . mostVisible $ asteroidMap

