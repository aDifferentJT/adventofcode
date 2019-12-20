{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}

import Maze (Maze, PortalType(..), Pos, addReached, findPortal, isPortal', isUnreached, parseMaze)

import Debug.Trace

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f (Just x)
  | f x       = Just x
  | otherwise = Nothing

headOrError :: String -> [a] -> a
headOrError e []    = error e
headOrError _ (x:_) = x

type RecursiveMaze = [Maze]
type RecursivePos = (Int, Pos)

isUnreached' :: RecursiveMaze -> RecursivePos -> Bool
isUnreached' m (d, p) = isUnreached (m !! d) p

unreachedAdjacents :: RecursiveMaze -> RecursivePos -> Set RecursivePos
unreachedAdjacents m (d, p@(x, y)) = Set.fromList (maybe id (:) portal directs)
  where directs :: [RecursivePos]
        directs = filter (isUnreached' m)
          [ (d, (x + 1, y))
          , (d, (x - 1, y))
          , (d, (x, y + 1))
          , (d, (x, y - 1))
          ]
        portal :: Maybe RecursivePos
        portal = Map.lookup p (head m) >>= \case
          (isPortal' -> Just (_, "AA")) -> Nothing
          (isPortal' -> Just (_, "ZZ")) -> Nothing
          (isPortal' -> Just (t, s))    ->
            let p' = headOrError ("Cannot find portal " ++ s) . filter (/= p) . findPortal s . head $ m in
            let d' = case t of
                  Interior -> d + 1
                  Exterior -> d - 1
            in
            if d' < 0
            then Nothing
            else if isUnreached' m (d', p')
            then Just (d', p')
            else Nothing
          _                       -> Nothing

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust _ _ []     = []
adjust f 0 (x:xs) = f x : xs
adjust f n (x:xs) = x : adjust f (n - 1) xs

addReached' :: RecursivePos -> RecursiveMaze -> RecursiveMaze
addReached' (d, p) = adjust (addReached p) d

startForMaze :: Maze -> Pos
startForMaze = headOrError "Cannot find start" . findPortal "AA"

destForMaze :: Maze -> Pos
destForMaze = headOrError "Cannot find destination" . findPortal "ZZ"

minDistance :: Maze -> Int
minDistance m = minDistance' (Set.singleton (0, startForMaze m)) 0 (repeat m) (0, destForMaze m)
  where minDistance' :: Set RecursivePos -> Int -> RecursiveMaze -> RecursivePos -> Int
        minDistance' latestReached distance maze dest
          | Set.member dest latestReached = distance
          | otherwise =
              let newReached = Set.unions . map (unreachedAdjacents maze) . Set.toList $ latestReached in
              let maze' = Set.foldr addReached' maze newReached in
              minDistance' newReached (distance + 1) maze' dest

main :: IO ()
main = do
  initialMaze <- parseMaze <$> readFile "input.txt"
  print . minDistance $ initialMaze

