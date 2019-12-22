{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Data.List (elemIndex, foldl', intercalate, uncons)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Text.Parsec (choice, endBy, many1, try)
import Text.Parsec.Char (digit, newline, space, string)
import Text.Parsec.String (Parser, parseFromFile)

zipListWith :: ([a] -> b) -> [[a]] -> [b]
zipListWith f = maybe [] (uncurry (:) . (f *** zipListWith f) . unzip) . sequence . map uncons 

newtype Card = Card Int
  deriving (Enum, Eq, Num, Show)

type Deck = [Card]

factory :: Deck
factory = [0..10006]

type Technique = Deck -> Deck

dealIntoNewStack :: Technique
dealIntoNewStack = reverse

cut :: Int -> Technique
cut n
  | n >= 0    = (uncurry . flip $ (++)) . splitAt n
  | otherwise = \deck -> cut (length deck + n) deck

dealWithIncrement :: Int -> Technique
dealWithIncrement n = \deck ->
  map fromJust
  . zipListWith (foldl' (<|>) Nothing)
  . chunksOf (length deck)
  . concatMap ((: replicate (n - 1) Nothing) . Just)
  $ deck

parseInt :: Parser Int
parseInt = do
  s <- choice [string "-", return ""]
  ds <- many1 digit
  return . read $ s ++ ds

parseTechnique :: Parser Technique
parseTechnique = foldl' (flip (.)) id <$> endBy parseTechnique' newline
  where parseTechnique' :: Parser Technique
        parseTechnique' = choice
          [ try $ do
              string "deal into new stack"
              return dealIntoNewStack
          , try $ do
              string "cut"
              space
              cut <$> parseInt
          , try $ do
              string "deal with increment"
              space
              dealWithIncrement <$> parseInt
          ]

main :: IO ()
main = parseFromFile parseTechnique "input.txt" >>= \case
  Left e -> print e
  Right t -> print . fromJust . elemIndex (Card 2019) . t $ factory

