import Data.List
import Data.List.Split
import Data.Map (Map, fromListWith, intersectionWith, toList)
import qualified Data.Map as Map
import System.IO

data Move
  = MoveLeft Int
  | MoveRight Int
  | MoveUp Int
  | MoveDown Int

parseMove :: String -> Move
parseMove ('L':s) = MoveLeft  . read $ s
parseMove ('R':s) = MoveRight . read $ s
parseMove ('U':s) = MoveUp    . read $ s
parseMove ('D':s) = MoveDown  . read $ s

parsePath :: String -> [Move]
parsePath = map parseMove . splitOn ","

getSquares :: Int -> Int -> Int -> [Move] -> [((Int, Int), Int)]
getSquares l x y [] = [((x, y), l)]
getSquares l x y (MoveLeft  0 : ms) = getSquares l x y ms
getSquares l x y (MoveLeft  n : ms) = ((x, y), l) : getSquares (l + 1) (x - 1) y (MoveLeft (n - 1) : ms)
getSquares l x y (MoveRight 0 : ms) = getSquares l x y ms
getSquares l x y (MoveRight n : ms) = ((x, y), l) : getSquares (l + 1) (x + 1) y (MoveRight (n - 1) : ms)
getSquares l x y (MoveUp    0 : ms) = getSquares l x y ms
getSquares l x y (MoveUp    n : ms) = ((x, y), l) : getSquares (l + 1) x (y + 1) (MoveUp (n - 1) : ms)
getSquares l x y (MoveDown  0 : ms) = getSquares l x y ms
getSquares l x y (MoveDown  n : ms) = ((x, y), l) : getSquares (l + 1) x (y - 1) (MoveDown (n - 1) : ms)

getCrossings :: [Move] -> [Move] -> Map (Int, Int) Int
getCrossings ms1 ms2 =
  intersectionWith (+)
    (fromListWith min . getSquares 0 0 0 $ ms1)
    (fromListWith min . getSquares 0 0 0 $ ms2)

taxicab :: (Int, Int) -> Int
taxicab (x, y) = abs x + abs y

main :: IO ()
main = do
  f <- openFile "input.txt" ReadMode
  p1 <- parsePath <$> hGetLine f
  p2 <- parsePath <$> hGetLine f
  print . minimum . map snd . toList . Map.filterWithKey (\k _ -> k /= (0, 0)) $ getCrossings p1 p2

