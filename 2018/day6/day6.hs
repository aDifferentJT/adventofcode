{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Control.Arrow ((***), first, second)
import Data.Bool (bool)
import Data.List (elemIndex, intercalate, intersperse, minimumBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)

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

boundaries :: Box -> [Coord]
boundaries Box{..} = concat
  [ [ (left, y) | y <- [top + 1 .. bottom - 1] ]
  , [ (right, y) | y <- [top + 1 .. bottom - 1] ]
  , [ (x, top) | x <- [left + 1 .. right - 1] ]
  , [ (x, bottom) | x <- [left + 1 .. right - 1] ]
  , [ (left, top)
    , (left, bottom)
    , (right, top)
    , (right, bottom)
    ]
  ]

grid :: Box -> [Coord]
grid Box{..} =
  [(x, y) | x <- [left + 1 .. right - 1], y <- [top + 1 .. bottom - 1]]

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

finiteAreas :: [Coord] -> Box -> [Coord]
finiteAreas cs = Set.toList . Set.difference (Set.fromList cs) . Set.fromList . mapMaybe (closest cs) . boundaries

countArea :: [Coord] -> Map Coord Int
countArea cs =
  let box = getBoundingBox cs in
  foldr (Map.adjust (+1)) (Map.fromList . map (,0) $ finiteAreas cs box) (areaGrid cs box)

keyWithMaxVal :: forall k a. (Ord k, Ord a) => Map k a -> Maybe k
keyWithMaxVal = fmap fst . Map.foldrWithKey f Nothing
  where f :: k -> a -> Maybe (k, a) -> Maybe (k, a)
        f k a Nothing = Just (k, a)
        f k a (Just (k', a'))
          | a' > a    = Just (k', a')
          | otherwise = Just (k, a)

coords :: [Coord]
coords = map ((\[x, y] -> (read x, read y) :: Coord) . splitOn ", ") . lines . unsafePerformIO . readFile $ "input.txt"

padLeft :: a -> Int -> [a] -> [a]
padLeft p n xs = case padLeft' p n xs of
    Left ys -> ys
    Right m -> drop m xs
  where padLeft' :: a -> Int -> [a] -> Either [a] Int
        padLeft' _ 0 xs = Right . length $ xs
        padLeft' p n xs = case padLeft' p (n - 1) xs of
          Left ys -> Left $ p:ys
          Right 0 -> Left $ p:xs
          Right m -> Right (m - 1)

padRight :: a -> Int -> [a] -> [a]
padRight p n = take n . (++ repeat p)

intersperseWithAdjacentAndMap :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
intersperseWithAdjacentAndMap _ _ []  = []
intersperseWithAdjacentAndMap _ g [x] = [g x]
intersperseWithAdjacentAndMap f g (x1:xs@(x2:_)) = g x1 : f x1 x2 : intersperseWithAdjacentAndMap f g xs

showTopBorder :: Int -> Int -> String
showTopBorder w n = "\x250C" ++ intercalate "\x252C" [ replicate w '\x2500' | _ <- [1..n] ] ++ "\x2510"

showMiddleBorder :: Int -> [(Bool, a)] -> [(Bool, a)] -> String
showMiddleBorder w xs ys = "\x251C" ++ (concat . intersperseWithAdjacentAndMap corner (replicate w . uncurry hBar) . map (fst *** fst) $ zip xs ys) ++ "\x2524"
  where hBar :: Bool -> Bool -> Char
        hBar False False = '\x2500'
        hBar True  False = '\x2580'
        hBar False True  = '\x2584'
        hBar True  True  = '\x2588'
        corner :: (Bool, Bool) -> (Bool, Bool) -> String
        corner (False, False) (False, False) = "\x253C"
        corner (True,  False) (False, False) = "\x2598"
        corner (False, True)  (False, False) = "\x2596"
        corner (False, False) (True,  False) = "\x259D"
        corner (False, False) (False, True)  = "\x2597"
        corner (True,  True)  (False, False) = "\x258C"
        corner (True,  False) (True,  False) = "\x2580"
        corner (True,  False) (False, True)  = "\x259A"
        corner (False, True)  (True,  False) = "\x259E"
        corner (False, True)  (False, True)  = "\x2584"
        corner (False, False) (True,  True)  = "\x2590"
        corner (True,  True)  (True,  False) = "\x259B"
        corner (True,  True)  (False, True)  = "\x2599"
        corner (True,  False) (True,  True)  = "\x259C"
        corner (False, True)  (True,  True)  = "\x259F"
        corner (True,  True)  (True,  True)  = "\x2588"

showBottomBorder :: Int -> Int -> String
showBottomBorder w n = "\x2514" ++ intercalate "\x2534" [ replicate w '\x2500' | _ <- [1..n] ] ++ "\x2518"

highlight :: String -> String
highlight = ("\ESC[7m" ++) . (++ "\ESC[m")

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

fillLeft :: Char -> Char
fillLeft '\x2502' = '\x258C'
fillLeft '\x2590' = '\x2588'

showRow :: Int -> [(Bool, String)] -> String
showRow w [] = "\x2502"
showRow w ((True,  x):cs) = "\x2590" ++ highlight (padLeft ' ' w x) ++ (mapHead fillLeft . showRow w $ cs)
showRow w ((False, x):cs) = "\x2502" ++ padLeft ' ' w x ++ showRow w cs

showGrid :: String
showGrid = 
  let Box{..} = getBoundingBox coords in
  let boxWidth = right - left in
  let boxHeight = bottom - top in
  let width = length . show . subtract 1 . length $ coords in
  unlines
    [ showTopBorder width boxWidth
    , intercalate "\n" . intersperseWithAdjacentAndMap (showMiddleBorder width) (showRow width) $
      [ [ let c = (x, y) in
          ( c `elem` coords
          , maybe "." (show . fromJust . flip elemIndex coords) (closest coords c)
          )
          | x <- [left..right]
        ]
        | y <- [top..bottom]
      ]
    , showBottomBorder width boxWidth
    ]

totalDistance :: [Coord] -> Coord -> Int
totalDistance cs c = sum . map (taxicab c) $ cs

main :: IO ()
main = do
  let areas = countArea coords
  let Just bestCoord = keyWithMaxVal areas
  print . Map.lookup bestCoord $ areas
  print . length . filter (< 10000) . map (totalDistance coords) . grid . getBoundingBox $ coords

