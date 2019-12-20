{-# LANGUAGE LambdaCase, ViewPatterns #-}

import Maze (Maze, Pos, addReached, findPortal, isPortal, isUnreached, parseMaze)

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

unreachedAdjacents :: Maze -> Pos -> Set Pos
unreachedAdjacents m p@(x, y) = Set.fromList (maybe id (:) portal directs)
  where directs :: [Pos]
        directs = filter (isUnreached m)
          [ (x + 1, y)
          , (x - 1, y)
          , (x, y + 1)
          , (x, y - 1)
          ]
        portal :: Maybe Pos
        portal = Map.lookup p m >>= \case
          (isPortal -> Just "AA") -> Nothing
          (isPortal -> Just "ZZ") -> Nothing
          (isPortal -> Just s)    -> filterMaybe (isUnreached m) . Just . headOrError ("Cannot find portal " ++ s) . filter (/= p) . findPortal s $ m
          _                       -> Nothing

startForMaze :: Maze -> Pos
startForMaze = headOrError "Cannot find start" . findPortal "AA"

destForMaze :: Maze -> Pos
destForMaze = headOrError "Cannot find destination" . findPortal "ZZ"

minDistance :: Maze -> Int
minDistance m = minDistance' (Set.singleton (startForMaze m)) 0 m (destForMaze m)
  where minDistance' :: Set Pos -> Int -> Maze -> Pos -> Int
        minDistance' latestReached distance maze dest
          | Set.member dest latestReached = distance
          | otherwise =
              let newReached = Set.unions . map (unreachedAdjacents maze) . Set.toList $ latestReached in
              let maze' = Set.foldr addReached maze newReached in
              minDistance' newReached (distance + 1) maze' dest

main :: IO ()
main = do
  initialMaze <- parseMaze <$> readFile "input.txt"
  print . minDistance $ initialMaze

