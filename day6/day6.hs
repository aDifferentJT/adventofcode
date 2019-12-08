import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Maybes (firstJusts)
import System.IO

data Body
  = Body String [Body]
  deriving Show

parseMap :: [(String, String)] -> Body
parseMap = parseBody "COM"

parseBody :: String -> [(String, String)] -> Body
parseBody b m = Body b . mapMaybe f $ m
  where f :: (String, String) -> Maybe Body
        f (b', b'')
          | b == b'   = Just . parseBody b'' $ m
          | otherwise = Nothing

countOrbits :: Body -> Int
countOrbits = sum . countOrbits'
  where countOrbits' :: Body -> [Int]
        countOrbits' (Body _ bs) = 0 : concatMap (map (+1) . countOrbits') bs

pathFromRoot :: Body -> String -> Maybe [String]
pathFromRoot (Body root []) dest
  | root == dest = Just []
  | otherwise    = Nothing
pathFromRoot (Body root bs) dest
  | root == dest = Just []
  | otherwise    = (root:) <$> (firstJusts . map (flip pathFromRoot dest) $ bs)

stripCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
stripCommonPrefix (x:xs) (y:ys)
  | x == y    = stripCommonPrefix xs ys
  | otherwise = (x:xs, y:ys)
stripCommonPrefix xs ys = (xs, ys)

distance :: Body -> String -> String -> Maybe Int
distance root = curry
  ( fmap
    ( uncurry (+)
    . (length *** length)
    )
  . (uncurry . liftA2 $ stripCommonPrefix)
  . (pathFromRoot root *** pathFromRoot root)
  )

main :: IO ()
main = do
  m <- parseMap . map ((\[x,y]->(x,y)) . splitOn ")") . lines <$> readFile "input.txt"
  print . countOrbits $ m
  print $ distance m "YOU" "SAN"

