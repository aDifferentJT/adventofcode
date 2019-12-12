
import Control.Arrow (first)
import Data.Maybe (mapMaybe)
import System.IO.Unsafe (unsafePerformIO)

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(x:_)  !!? 0 = Just x
(_:xs) !!? n = xs !!? (n - 1)

type Metadata = Int
data Node = Node [Node] [Metadata]
  deriving Show

parseNode :: [Int] -> (Node, [Int])
parseNode (c:m:xs) = let (cs, xs') = parseChildren c xs in (Node cs (take m xs'), drop m xs')
  where parseChildren :: Int -> [Int] -> ([Node], [Int])
        parseChildren 0 xs = ([], xs)
        parseChildren n xs = let (c, xs') = parseNode xs in first (c:) . parseChildren (n - 1) $ xs'

sumMetadata :: Node -> Int
sumMetadata (Node cs ms) = (sum . map sumMetadata $ cs) + sum ms

value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum . mapMaybe (fmap value . (cs !!?) . subtract 1) $ ms

{-# NOINLINE root #-}
root :: Node
(root, []) = parseNode . map read . words . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  print . sumMetadata $ root
  print . value $ root

