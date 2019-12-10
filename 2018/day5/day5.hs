{-# LANGUAGE ViewPatterns #-}

import Control.Arrow (second)
import Data.Char (toLower, ord)
import qualified Data.Set as Set

areOpposites :: Char -> Char -> Bool
areOpposites x y = abs (ord x - ord y) == 32

reduceOnce :: String -> (Bool, String)
reduceOnce "" = (False, "")
reduceOnce [c] = (False, [c])
reduceOnce (c1:c2:cs)
  | areOpposites c1 c2 = (True, snd . reduceOnce $ cs)
  | otherwise          = second (c1:) . reduceOnce $ c2:cs

reduceFully :: String -> String
reduceFully (reduceOnce -> (False, s)) = s
reduceFully (reduceOnce -> (True,  s)) = reduceFully s

getAllTypes :: String -> String
getAllTypes = Set.toList . Set.fromList . map toLower

removeAllOfType :: Char -> String -> String
removeAllOfType _ "" = ""
removeAllOfType t (c:cs)
  | toLower c == t = removeAllOfType t cs
  | otherwise = c : removeAllOfType t cs

main :: IO ()
main = do
  p <- filter (/= '\n') <$> readFile "input.txt"
  print . length . reduceFully $ p
  print . minimum . map (length . reduceFully . flip removeAllOfType p) . getAllTypes $ p

