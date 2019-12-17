{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Arrow ((***), (&&&), first, second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString(fromString))
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

parseInteger :: Parser Integer
parseInteger = read <$> many1 digit

type Chemical = (Integer, String)

parseReactions :: Parser (Map String (Integer, [(String, Integer)]))
parseReactions = Map.fromList <$> many parseReaction
  where parseReaction :: Parser (String, (Integer, [(String, Integer)]))
        parseReaction = do
          reqList <- parseRequirements
          spaces
          string "=>"
          spaces
          (c, n) <- parseString
          endOfLine
          return (c, (n, reqList))
        parseRequirements :: Parser [(String, Integer)]
        parseRequirements = sepBy1 parseString (char ',' >> spaces)
        parseString :: Parser (String, Integer)
        parseString = do
          n <- parseInteger
          space
          c <- many1 upper
          return (c, n)

divCeil :: Integer -> Integer -> Integer
divCeil x y =
  let q = div x y in
  if q * y == x
  then q
  else q + 1

calculateNeeded' :: Map Chemical (Integer, [(Chemical, Integer)]) -> Map Chemical Integer -> Integer
calculateNeeded' rs needed =
  case Map.maxViewWithKey needed of
    Just (((_, "ORE"), amountNeeded), _) -> amountNeeded
    Just ((chemical, amountNeeded), needed') ->
      let Just (amountProduced, reqs) = Map.lookup chemical rs in
      calculateNeeded' rs
      . Map.unionWith (+) needed'
      . Map.fromList
      . map (second (* divCeil amountNeeded amountProduced))
      $ reqs

binSearch :: Integral a => (a -> Ordering) -> a -> a -> a
binSearch f x y
  | x == y    =
      case f x of
        LT -> x - 1
        EQ -> x
        GT -> x
  | otherwise =
      let m = (x + y) `div` 2 in
      case f m of
        LT -> binSearch f x (m - 1)
        EQ -> m
        GT -> binSearch f (m + 1) y

{-# NOINLINE reactions #-}
reactions :: Map String (Integer, [(String, Integer)])
reactions = either (error . show) id . unsafePerformIO . parseFromFile parseReactions $ "input.txt"

weights :: Map String Integer
weights = Map.insert "ORE" 0 . Map.fromList . map (id &&& weightOfChemical) . Map.keys $ reactions
  where weightOfChemical :: String -> Integer
        weightOfChemical c =
          let Just (_, reqs) = Map.lookup c reactions in
          (+1) . maximum . map (fromJust . flip Map.lookup weights . fst) $ reqs

weightedReactions :: Map Chemical (Integer, [(Chemical, Integer)])
weightedReactions =
  Map.fromList
  . map
    (   (   fromJust . flip Map.lookup weights
        &&& id
        )
    *** ( second
        . map
        . first
        $ fromJust . flip Map.lookup weights &&& id
        )
    )
  . Map.toList
  $ reactions

calculateNeeded :: Integer -> Integer
calculateNeeded = calculateNeeded' weightedReactions . Map.fromList . (:[]) . ((fromJust . Map.lookup "FUEL" $ weights, "FUEL"),)

main :: IO ()
main = do
  let oreFor1 = calculateNeeded 1
  print oreFor1
  print $ binSearch (compare (10^12) . calculateNeeded) (10^12 `div` oreFor1) (10^13 `div` oreFor1)

