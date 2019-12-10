
import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

type Image = [Layer]
type Layer = [Row]
type Row = [Int]

parseImage :: Int -> Int -> [Int] -> Image
parseImage w h = chunksOf h . chunksOf w

countNs :: Int -> Layer -> Int
countNs n = length . filter (== n) . concat

minimumWith :: Ord o => (a -> o) -> [a] -> a
minimumWith f = minimumBy . comparing

layerWithFewest0s :: Image -> Layer
layerWithFewest0s = minimumWith (countNs 0)

onesMulTwos :: Layer -> Int
onesMulTwos l = countNs 1 l * countNs 2 l

combinePixels :: [Int] -> Int
combinePixels  []    = 2
combinePixels (2:xs) = combinePixels xs
combinePixels (x:_)  = x

zipList :: ([a] -> b) -> [[a]] -> [b]
zipList f layers =
  if null . head $ layers
  then []
  else f (map head layers) : zipList f (map tail layers)

combineLayers :: Image -> Layer
combineLayers = zipList (zipList combinePixels)

showLayer :: Layer -> String
showLayer = unlines . map showRow
  where showRow :: Row -> String
        showRow = map showPixel
        showPixel :: Int -> Char
        showPixel 0 = ' '
        showPixel 1 = '\x2588'
        showPixel 2 = '\x2591'

main :: IO ()
main = do
  i <- parseImage 25 6 . map (read . (:[])) . filter (/= '\n') <$> readFile "input.txt"
  print . onesMulTwos . layerWithFewest0s $ i
  putStr . showLayer . combineLayers $ i

