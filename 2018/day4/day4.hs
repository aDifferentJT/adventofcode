{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}

import Control.Arrow (first)
import Control.Monad (unless, void)
import Data.List (sort, sortOn)
import Data.List.Split (chunksOf, dropInitBlank, keepDelimsL, split, whenElt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadPrec
import Text.Read (readPrec)

import Debug.Trace (trace)

data TimeStamp = TimeStamp
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  }
  deriving (Eq, Ord, Show)

pchar :: Char -> ReadPrec ()
pchar c = do
  c' <- get
  unless (c == c') pfail

pstring :: String -> ReadPrec ()
pstring = void . traverse pchar

instance Read TimeStamp where
  readPrec = do
    pstring "["
    year <- readPrec
    pstring "-"
    month <- readPrec
    pstring "-"
    day <- readPrec
    pstring " "
    hour <- readPrec
    pstring ":"
    minute <- readPrec
    pstring "]"
    return TimeStamp{..}

data Message
  = BeginShift { guard :: Int, time :: TimeStamp }
  | FallAsleep { time :: TimeStamp }
  | WakeUp { time :: TimeStamp }
  deriving Show

isBeginShift :: Message -> Bool
isBeginShift BeginShift{} = True
isBeginShift _ = False

instance Read Message where
  readPrec = do
      time <- readPrec
      pstring " "
      ($ time) <$> (readBeginShift +++ readFallAsleep +++ readWakeUp)
    where readBeginShift :: ReadPrec (TimeStamp -> Message)
          readBeginShift = do
            pstring "Guard #"
            guard <- readPrec
            pstring " begins shift"
            return (BeginShift guard)
          readFallAsleep :: ReadPrec (TimeStamp -> Message)
          readFallAsleep = do
            pstring "falls asleep"
            return FallAsleep
          readWakeUp :: ReadPrec (TimeStamp -> Message)
          readWakeUp = do
            pstring "wakes up"
            return WakeUp

splitIntoGuards :: [Message] -> [[Message]]
splitIntoGuards = split . dropInitBlank . keepDelimsL . whenElt $ isBeginShift

getGuardTimesForNight :: [Message] -> (Int, [(Int, Int)])
getGuardTimesForNight (BeginShift{guard}:ms) = (guard, map (\[FallAsleep {time=t1}, WakeUp {time=t2}] -> (minute t1, minute t2)) . chunksOf 2 $ ms)

getGuardTimes :: Ord k => [(k, [a])] -> Map k [a]
getGuardTimes = Map.fromListWith (++)

numMinutesAsleep :: [(Int, Int)] -> Int
numMinutesAsleep = sum . map (uncurry . flip $ (-))

keyWithMaxVal :: forall k a. (Ord k, Ord a) => Map k a -> Maybe k
keyWithMaxVal = fmap fst . Map.foldrWithKey f Nothing
  where f :: k -> a -> Maybe (k, a) -> Maybe (k, a)
        f k a Nothing = Just (k, a)
        f k a (Just (k', a'))
          | a' > a    = Just (k', a')
          | otherwise = Just (k, a)

mapWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhile _ _ [] = []
mapWhile p f (x:xs)
  | p x = f x : mapWhile p f xs
  | otherwise = x:xs

mostAsleepMinute :: [(Int, Int)] -> Maybe (Int, Int)
mostAsleepMinute = mostAsleepMinute' . sort
  where mostAsleepMinute' :: [(Int, Int)] -> Maybe (Int, Int)
        mostAsleepMinute' [] = Nothing
        mostAsleepMinute' xs =
          let x = fst . head $ xs in
          max
            (Just (length . takeWhile ((== x) . fst) $ xs, x))
            (mostAsleepMinute' . filter (uncurry (/=)) . mapWhile ((== x) . fst) (first (+1)) $ xs)

main :: IO ()
main = do
  ms <- getGuardTimes . map getGuardTimesForNight . splitIntoGuards . sortOn time . map (read :: String -> Message) . lines <$> readFile "input.txt"
  let Just sleepiestGuard = keyWithMaxVal . Map.map numMinutesAsleep $ ms
  let Just (_, bestMin) = mostAsleepMinute =<< Map.lookup sleepiestGuard ms
  print (sleepiestGuard * bestMin)
  let guardMostAsleepMinutes = Map.mapMaybe mostAsleepMinute ms
  let Just bestGuard' = keyWithMaxVal guardMostAsleepMinutes
  let Just (_, bestMin') = Map.lookup bestGuard' guardMostAsleepMinutes
  print (bestGuard' * bestMin')
  return ()

