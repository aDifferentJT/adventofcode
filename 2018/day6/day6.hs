{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Control.Arrow ((***), first, second)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

type Coord = (Int, Int)

data Box = Box
  { left   :: Int
  , right  :: Int
  , top    :: Int
  , bottom :: Int
  }

getBoundingBox :: [Coord] -> Box
getBoundingBox cs = Box
  { left   = minimum . map fst $ cs
  , right  = maximum . map fst $ cs
  , top    = minimum . map snd $ cs
  , bottom = maximum . map snd $ cs
  }

sign :: Int -> Int
sign n
  | n <  0 = -1
  | n == 0 = 0
  | n >  0 = 1
  | otherwise = undefined

isLineThroughNw :: [Coord] -> Coord -> Bool
isLineThroughNw cs (xc, yc) = or
  [ sign ((x1 - x2) * (y1 - xc - yc) - (y1 - y2) * x1) == sign (y1 - y2 + x1 - x2) | (x1, y1) <- cs, (x2, y2) <- cs ]

isLineThroughNe :: [Coord] -> Coord -> Bool
isLineThroughNe cs c = isLineThroughNw (map (first negate) cs) (first negate c)

isLineThroughSe :: [Coord] -> Coord -> Bool
isLineThroughSe cs c = isLineThroughNw (map (negate *** negate) cs) ((negate *** negate) c)

isLineThroughSw :: [Coord] -> Coord -> Bool
isLineThroughSw cs c = isLineThroughNw (map (second negate) cs) (second negate c)

isStrictlyWithinHull :: [Coord] -> Coord -> Bool
isStrictlyWithinHull cs c@(x, y) =
  let nwQuad = filter (\(x', y') -> x' < x && y' < y) cs in
  let neQuad = filter (\(x', y') -> x' > x && y' < y) cs in
  let seQuad = filter (\(x', y') -> x' > x && y' > y) cs in
  let swQuad = filter (\(x', y') -> x' < x && y' > y) cs in
  case (nwQuad, neQuad, seQuad, swQuad) of
    (_:_, _:_, _:_, _:_) -> True
    ([], [], _, _) -> False
    (_, [], [], _) -> False
    (_, _, [], []) -> False
    ([], _, _, []) -> False
    ([], _:_, [], _:_) -> isLineThroughNw cs c && isLineThroughSe cs c
    (_:_, [], _:_, []) -> isLineThroughNe cs c && isLineThroughSw cs c
    ([], _:_, _:_, _:_) -> isLineThroughNw cs c
    (_:_, [], _:_, _:_) -> isLineThroughNe cs c
    (_:_, _:_, [], _:_) -> isLineThroughSe cs c
    (_:_, _:_, _:_, []) -> isLineThroughSw cs c

finiteAreas :: [Coord] -> [Coord]
finiteAreas cs = filter (isStrictlyWithinHull cs) cs

grid :: Box -> [Coord]
grid Box{..} =
  let w = right - left in
  let h = bottom - top in
  [(x, y) | x <- [left - w .. right + w], y <- [top - h .. bottom + h]]

taxicab :: Coord -> Coord -> Int
taxicab (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith = minimumBy . comparing

closest :: [Coord] -> Coord -> Maybe Coord
closest cs c =
  let c' = minimumWith (taxicab c) cs in
  if taxicab c c' == (taxicab c . minimumWith (taxicab c) . filter (/= c') $ cs)
  then Nothing
  else Just c'

areaGrid :: [Coord] -> Box -> [Coord]
areaGrid cs = mapMaybe (closest cs) . grid

countArea :: [Coord] -> Map Coord Int
countArea cs =
  foldr (Map.adjust (+1)) (Map.fromList . map (,0) . finiteAreas $ cs) . areaGrid cs . getBoundingBox $ cs

keyWithMaxVal :: forall k a. (Ord k, Ord a) => Map k a -> Maybe k
keyWithMaxVal = fmap fst . Map.foldrWithKey f Nothing
  where f :: k -> a -> Maybe (k, a) -> Maybe (k, a)
        f k a Nothing = Just (k, a)
        f k a (Just (k', a'))
          | a' > a    = Just (k', a')
          | otherwise = Just (k, a)

main :: IO ()
main = do
  cs <- map ((\[x, y] -> (read x, read y) :: Coord) . splitOn ", ") . lines <$> readFile "input.txt"
  let areas = countArea cs
  let Just bestCoord = keyWithMaxVal areas
  print . Map.lookup bestCoord $ areas

