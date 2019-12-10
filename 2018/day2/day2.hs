
import Control.Arrow ((***))
import Data.Foldable (asum)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

counts :: Ord a => [a] -> Map a Int
counts = foldr (\x -> Map.insertWith (+) x 1) Map.empty

checksum :: [String] -> Int
checksum is = check2 * check3
  where check2 :: Int
        check3 :: Int
        (check2, check3) =
          foldr
            (\cs (c2, c3) ->
              ( c2 + fromEnum (2 `elem` cs)
              , c3 + fromEnum (3 `elem` cs)
              )
            )
            (0, 0)
            css
        css :: [[Int]]
        css = map (Map.elems . counts) is

duplicate :: Ord b => (a -> b) -> [a] -> Maybe (a, a)
duplicate f = duplicate' f . sortOn f
  where duplicate' :: Eq b => (a -> b) -> [a] -> Maybe (a, a)
        duplicate' f []  = Nothing
        duplicate' f [_] = Nothing
        duplicate' f (x1:xs@(x2:_))
          | f x1 == f x2 = Just (x1, x2)
          | otherwise    = duplicate' f xs

findDiff1 :: Ord a => [[a]] -> Maybe ([a], [a])
findDiff1 xss = asum
  [ duplicate tail xss
  , asum
    . map
      (\xss' ->
        fmap (let x = head . head $ xss' in (x:) *** (x:))
        . findDiff1
        . map tail
        $ xss'
      )
      . filter ((>1) . length)
      . groupOn head
      . sortOn head
      $ xss
  ]

commonElems :: Eq a => [a] -> [a] -> [a]
commonElems [] [] = []
commonElems (x:xs) (y:ys)
  | x == y    = x : commonElems xs ys
  | otherwise = commonElems xs ys

main :: IO ()
main = do
  is <- lines <$> readFile "input.txt"
  print . checksum $ is
  print . (fmap . uncurry $ commonElems) . findDiff1 $ is

