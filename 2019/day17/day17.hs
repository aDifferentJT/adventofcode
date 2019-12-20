{-# LANGUAGE MultiWayIf, RecordWildCards, TupleSections #-}

import Data.Trie (Trie)
import qualified Data.Trie as Trie
import IntCode (parseProgram, runProgramLists)

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Char (chr, ord)
import Data.List (intercalate, intersect, intersperse, findIndex, foldl', unfoldr)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

{-# NOINLINE cameraView #-}
cameraView :: [String]
cameraView = filter (not . null) . lines . map (chr . fromInteger) . runProgramLists program $ []

{-# NOINLINE cameraViewMap #-}
cameraViewMap :: Map (Int, Int) Char
cameraViewMap = Map.fromList . concat . zipWith (\y -> zipWith (\x -> ((x, y),)) [0..]) [0..] $ cameraView

example :: [String]
example =
  [ "..#.........."
  , "..#.........."
  , "#######...###"
  , "#.#...#...#.#"
  , "#############"
  , "..#...#...#.."
  , "..#####...^.."
  ]

scaffold :: [String] -> [[Bool]]
scaffold = map (map (`elem` "#^v<>"))

surroundedTB :: [Bool] -> [Bool] -> [Bool] -> [Bool]
surroundedTB xs ys zs = foldl' (zipWith (&&)) (repeat True) [xs, ys, zs]

surroundedLR :: [Bool] -> [Bool]
surroundedLR xs@(_:ys@(_:zs)) = surroundedTB xs ys zs

intersections :: [[Bool]] -> [[Bool]]
intersections xss@(_:yss@(_:zss)) = zipWith ($) (zipWith surroundedTB (map tail xss) (map surroundedLR yss)) (map tail zss)

sumAlignmentParams :: [[Bool]] -> Int
sumAlignmentParams = sum . map sum . zipWith (\x -> zipWith (\y b -> if b then x * y else 0) [1..]) [1..]

data Instruction
  = TurnLeft
  | TurnRight
  | MoveForward Int
  deriving (Eq, Ord)

instance Show Instruction where
  show TurnLeft        = "L"
  show TurnRight       = "R"
  show (MoveForward n) = show n

lengthOfInstruction :: Instruction -> Int
lengthOfInstruction = length . show

type Path = [Instruction]

showPath = intercalate "," . map show

compactPath :: Path -> Path
compactPath [] = []
compactPath (MoveForward x : MoveForward y : p) = compactPath (MoveForward (x + y) : p)
compactPath (i:p) = i : compactPath p

data Direction
  = UpDir
  | DownDir
  | LeftDir
  | RightDir
  deriving Show

turnLeft :: Direction -> Direction
turnLeft UpDir    = LeftDir
turnLeft DownDir  = RightDir
turnLeft LeftDir  = DownDir
turnLeft RightDir = UpDir

turnRight :: Direction -> Direction
turnRight UpDir    = RightDir
turnRight DownDir  = LeftDir
turnRight LeftDir  = UpDir
turnRight RightDir = DownDir

moveIn :: (Int, Int) -> Direction -> (Int, Int)
moveIn (x, y) UpDir    = (x, y - 1)
moveIn (x, y) DownDir  = (x, y + 1)
moveIn (x, y) LeftDir  = (x - 1, y)
moveIn (x, y) RightDir = (x + 1, y)

data Robot = Robot
  { pos :: (Int, Int)
  , dir :: Direction
  }

initialRobot :: Robot
initialRobot =
  let Just y = findIndex (not . null . intersect "^v<>") cameraView in
  let Just x = findIndex (`elem` "^v<>") (cameraView !! y) in
  let dir = case cameraViewMap ! (x, y) of
        '^' -> UpDir
        'v' -> DownDir
        '<' -> LeftDir
        '>' -> RightDir
  in
  Robot
    { pos = (x, y)
    , ..
    }

splitAtWith :: (a -> Int) -> Int -> [a] -> ([a], [a])
splitAtWith _ _ [] = ([], [])
splitAtWith _ 0 xs = ([], xs)
splitAtWith f n (x:xs) =
  let fx = f x in if
    | fx <= n   ->  first (x:) . splitAtWith f (n - fx) $ xs
    | otherwise -> ([], x:xs)

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f (Just x)
  | f x       = Just x
  | otherwise = Nothing

compress :: Ord a => (a -> Int) -> Int -> Int -> [a] -> Maybe ([[a]], [Int])
compress lengthFunc lengthLimit numFuncs =
    ( fmap
    . first
    $ \t ->
      let m = Trie.inverseMap t in
      map (fromMaybe [] . flip Map.lookup m) [0 .. numFuncs - 1]
    ) . compress' Trie.empty 0 lengthFunc lengthLimit numFuncs

  where compress' :: Ord a => Trie a Int -> Int -> (a -> Int) -> Int -> Int -> [a] -> Maybe (Trie a Int, [Int])
        compress' funcs _            _          _           _            [] = Just (funcs, [])
        compress' funcs numFuncsUsed lengthFunc lengthLimit numFuncsLeft xs = case Trie.longestPrefix xs funcs of
          Just (xs', y) ->
            second (y:)
            <$> compress' funcs numFuncsUsed lengthFunc lengthLimit numFuncsLeft xs'
          Nothing       ->
            if numFuncsLeft == 0
            then Nothing
            else foldr
              ( (<|>)
              . (\n ->
                  let (xs1, xs2) = splitAtWith lengthFunc n xs in
                  compress'
                    (Trie.insert xs1 numFuncsUsed funcs)
                    (numFuncsUsed + 1)
                    lengthFunc
                    lengthLimit
                    (numFuncsLeft - 1)
                    xs2
                  >>= (filterMaybe ((<=lengthLimit) . length . snd) . Just . second (numFuncsUsed:))
                )
              )
              Nothing
              (reverse [1..lengthLimit])

path :: Path
path = compactPath . unfoldr f $ initialRobot
  where f :: Robot -> Maybe (Instruction, Robot)
        f Robot{..}
          | Map.lookup (moveIn pos dir)               cameraViewMap == Just '#'
            = Just (MoveForward 1, Robot{ pos = moveIn pos dir, .. })
          | Map.lookup (moveIn pos . turnLeft  $ dir) cameraViewMap == Just '#'
            = Just (TurnLeft,      Robot{ dir = turnLeft dir, .. })
          | Map.lookup (moveIn pos . turnRight $ dir) cameraViewMap == Just '#'
            = Just (TurnRight,     Robot{ dir = turnRight dir, .. })
          | otherwise = Nothing

robotInput :: String
robotInput =
  let Just (funcs, routine) = compress lengthOfInstruction 10 3 path in
  unlines . concat $
    [ [intersperse ',' . map (chr . (+ ord 'A')) $ routine]
    , map showPath funcs
    , ["n"]
    ]

runRobot :: Integer
runRobot = last . runProgramLists (2 : tail program) . map (fromIntegral . ord) $ robotInput

main :: IO ()
main = do
  print . sumAlignmentParams . intersections . scaffold $ cameraView
  print runRobot

