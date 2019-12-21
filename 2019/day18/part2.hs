{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
import Debug.Trace

import Control.Arrow ((&&&), second)
import Control.Monad.State (State, get, modify, runState)
import Data.Char (ord)
import qualified Data.List as List
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
  | Current Int
  deriving Show

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare '.' = Empty
parseSquare '@' = Current 0
parseSquare c
  | 'a' <= c && c <= 'z' = Key (ord c - ord 'a')
  | 'A' <= c && c <= 'Z' = Door (ord c - ord 'A')

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _     = False

isKey :: Square -> Maybe Int
isKey (Key k) = Just k
isKey _       = Nothing

isCurrent :: Square -> Maybe Int
isCurrent (Current c) = Just c
isCurrent _           = Nothing

makeReached :: Square -> Square
makeReached Empty = EmptyReached
makeReached sq    = error $ "Cannot make reached " ++ show sq

makeNotCurrent :: Int -> Square -> Square
makeNotCurrent x sq@(Current y)
  | x == y    = Empty
  | otherwise = sq
makeNotCurrent _ sq = sq

openDoor :: Int -> Square -> Square
openDoor k sq@(Door d)
  | k == d    = Empty
  | otherwise = sq
openDoor _ sq = sq

type Maze = Map Pos Square

addReached :: Pos -> Maze -> Maze
addReached = Map.adjust makeReached

removeCurrent :: Int -> Maze -> Maze
removeCurrent = Map.map . makeNotCurrent

moveToKey :: Int -> Pos -> Int -> Maze -> Maze
moveToKey c p k m =
  Map.insert p (Current c) . Map.map (openDoor k) $ m

adjacents :: Maze -> Pos -> Set Pos
adjacents m (x, y) = Set.fromList
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

splitIntoFour :: Maze -> Maze
splitIntoFour m =
  let [(x, y)] = Map.keys . Map.mapMaybe isCurrent $ m in
  flip Map.union m . Map.fromList $
    [ ((x - 1, y - 1), Current 1)
    , ((x - 1, y + 1), Current 2)
    , ((x + 1, y - 1), Current 3)
    , ((x + 1, y + 1), Current 4)
    , ((x, y - 1), Wall)
    , ((x, y + 1), Wall)
    , ((x - 1, y), Wall)
    , ((x + 1, y), Wall)
    , ((x, y), Wall)
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

startPos :: Maze -> [(Pos, Int)]
startPos = Map.toList . Map.mapMaybe isCurrent

setFlatFirst :: (Ord b, Ord c) => (a -> Set c) -> (a, b) -> Set (c, b)
setFlatFirst f (x, y) = Set.map (,y) . f $ x

accessibleKeys :: [(Pos, Int)] -> Maze -> [(((Pos, Int), Int), Int)]
accessibleKeys = accessibleKeys' 1 . Set.fromList
  where accessibleKeys' :: Int -> Set (Pos, Int) -> Maze -> [(((Pos, Int), Int), Int)]
        accessibleKeys' distance latestReached maze =
          let as = Set.unions . map (setFlatFirst (adjacents maze)) . Set.toList $ latestReached in
          let newReached = Set.filter (maybe False isEmpty . flip Map.lookup maze . fst) as in
          concat
            [ mapMaybe
                ( fmap (, distance)
                . uncurry (fmap . (,))
                . (id &&& isKey . (maze !) . fst)
                )
              . Set.toList
              $ as
            , if null newReached
              then []
              else
                let maze' = Set.foldr (addReached . fst) maze newReached in
                accessibleKeys' (distance + 1) newReached maze'
            ]

type Cached = State (Map (Set Int, [(Pos, Int)]) Int)

shortestPath :: Maze -> Int
shortestPath maze = fst (runState (shortestPath' Set.empty (startPos maze) maze) Map.empty)
  where shortestPath' :: Set Int -> [(Pos, Int)] -> Maze -> Cached Int
        shortestPath' collected currentPos maze =
          (Map.lookup (collected, currentPos) <$> get) >>= \case
            Just n  -> return n
            Nothing -> do
              ds <- mapM
                        (\(((p, c), k), d) ->
                          fmap (+d)
                          . shortestPath' (Set.insert k collected) ((p, c) : filter ((/= c) . snd) currentPos)
                          . moveToKey c p k
                          . removeCurrent c
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
  --print . shortestPath $ maze
  print . shortestPath . splitIntoFour $ maze

