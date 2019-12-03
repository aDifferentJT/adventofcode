import Data.List
import Data.List.Split
import Data.Set (Set, fromList, intersection, toList)
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

getSquares :: Int -> Int -> [Move] -> [(Int, Int)]
getSquares x y [] = [(x, y)]
getSquares x y (MoveLeft  0 : ms) = getSquares x y ms
getSquares x y (MoveLeft  n : ms) = (x, y) : getSquares (x - 1) y (MoveLeft (n - 1) : ms)
getSquares x y (MoveRight 0 : ms) = getSquares x y ms
getSquares x y (MoveRight n : ms) = (x, y) : getSquares (x + 1) y (MoveRight (n - 1) : ms)
getSquares x y (MoveUp    0 : ms) = getSquares x y ms
getSquares x y (MoveUp    n : ms) = (x, y) : getSquares x (y + 1) (MoveUp (n - 1) : ms)
getSquares x y (MoveDown  0 : ms) = getSquares x y ms
getSquares x y (MoveDown  n : ms) = (x, y) : getSquares x (y - 1) (MoveDown (n - 1) : ms)

getCrossings :: [Move] -> [Move] -> Set (Int, Int)
getCrossings ms1 ms2 =
  intersection
    (fromList . getSquares 0 0 $ ms1)
    (fromList . getSquares 0 0 $ ms2)

taxicab :: (Int, Int) -> Int
taxicab (x, y) = abs x + abs y

main :: IO ()
main = do
  f <- openFile "input.txt" ReadMode
  p1 <- parsePath <$> hGetLine f
  p2 <- parsePath <$> hGetLine f
  print . minimum . map taxicab . filter (/= (0, 0)) . toList $ getCrossings p1 p2

