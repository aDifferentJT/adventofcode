
import Data.Set (Set)
import qualified Data.Set as Set

firstDoubleF :: Set Integer -> Integer -> [Integer] -> Maybe Integer
firstDoubleF s f []
  | Set.member f s = Just f
  | otherwise = Nothing
firstDoubleF s f (d:ds)
  | Set.member f s = Just f
  | otherwise = firstDoubleF (Set.insert f s) (f + d) ds

main :: IO ()
main = do
  ds <- map (read . filter (/= '+')) . lines <$> readFile "input.txt"
  print . sum $ ds
  print . firstDoubleF Set.empty 0 . cycle $ ds

