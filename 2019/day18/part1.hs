{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}

import Control.Arrow ((&&&), second)
import Control.Monad.State (State, get, modify, runState)
import Data.Char (ord)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data PortalType
  = Interior
  | Exterior
  deriving Show

data Square
  = Wall
  | Empty
  | EmptyReached
  | Key Int
  | Door Int
  | Current
  deriving Show

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare '.' = Empty
parseSquare '@' = Current
parseSquare c
  | 'a' <= c && c <= 'z' = Key (ord c - ord 'a')
  | 'A' <= c && c <= 'Z' = Door (ord c - ord 'A')

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _     = False

isKey :: Square -> Maybe Int
isKey (Key k) = Just k
isKey _       = Nothing

isCurrent :: Square -> Bool
isCurrent Current = True
isCurrent _       = False

makeReached :: Square -> Square
makeReached Empty = EmptyReached
makeReached sq    = error $ "Cannot make reached " ++ show sq

makeNotCurrent :: Square -> Square
makeNotCurrent Current = Empty
makeNotCurrent sq      = sq

openDoor :: Int -> Square -> Square
openDoor k sq@(Door d)
  | k == d    = Empty
  | otherwise = sq
openDoor _ sq = sq

type Maze = Map Pos Square

addReached :: Pos -> Maze -> Maze
addReached = Map.adjust makeReached

removeCurrent :: Maze -> Maze
removeCurrent = Map.map makeNotCurrent

moveToKey :: Pos -> Int -> Maze -> Maze
moveToKey p k m =
  Map.insert p Current . Map.map (openDoor k) $ m

adjacents :: Maze -> Pos -> Set Pos
adjacents m (x, y) = Set.fromList
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

parseMaze :: String -> Maze
parseMaze =
  Map.fromList
  . map (second parseSquare)
  . concat
  . zipWith
      (\y -> zip (map (,y) [0..]))
      [0..]
  . filter (not . null)
  . lines

startPos :: Maze -> Pos
startPos = head . Map.keys . Map.filter isCurrent

accessibleKeys :: Pos -> Maze -> [((Pos, Int), Int)]
accessibleKeys = accessibleKeys' 1 . Set.singleton
  where accessibleKeys' :: Int -> Set Pos -> Maze -> [((Pos, Int), Int)]
        accessibleKeys' distance latestReached maze =
          let as = Set.unions . map (adjacents maze) . Set.toList $ latestReached in
          let newReached = Set.filter (maybe False isEmpty . flip Map.lookup maze) as in
          concat
            [ mapMaybe
                ( fmap (, distance)
                . uncurry (fmap . (,))
                . (id &&& isKey . (maze !))
                )
              . Set.toList
              $ as
            , if null newReached
              then []
              else
                let maze' = Set.foldr addReached maze newReached in
                accessibleKeys' (distance + 1) newReached maze'
            ]

type Cached = State (Map (Set Int, Pos) Int)

shortestPath :: Maze -> Int
shortestPath maze = fst (runState (shortestPath' Set.empty (startPos maze) maze) Map.empty)
  where shortestPath' :: Set Int -> Pos -> Maze -> Cached Int
        shortestPath' collected currentPos maze =
          (Map.lookup (collected, currentPos) <$> get) >>= \case
            Just n  -> return n
            Nothing -> do
              ds <- mapM
                        (\((p, k), d) ->
                          fmap (+d)
                          . shortestPath' (Set.insert k collected) p
                          . moveToKey p k
                          . removeCurrent
                          $ maze
                        )
                    . accessibleKeys currentPos
                    $ maze
              let res = case ds of
                    []  -> 0
                    _:_ -> minimum ds
              modify (Map.insert (collected, currentPos) res)
              return res

main :: IO ()
main = do
  maze <- parseMaze <$> readFile "input.txt"
  print . shortestPath $ maze

