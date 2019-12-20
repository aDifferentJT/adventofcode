{-# LANGUAGE TupleSections #-}

module Data.Trie (Trie, empty, singleton, insert, lookup, longestPrefix, inverseMap) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map

data Trie a b = Trie (Maybe b) (Map a (Trie a b))

empty :: Trie a b
empty = Trie Nothing Map.empty

singleton :: [a] -> b -> Trie a b
singleton []     = flip Trie Map.empty . Just
singleton (x:xs) = Trie Nothing . Map.singleton x . singleton xs

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert []     y (Trie _ m) = Trie (Just y) m
insert (x:xs) y (Trie z m) = Trie z . Map.alter (f xs y) x $ m
  where f :: Ord a => [a] -> b -> Maybe (Trie a b) -> Maybe (Trie a b)
        f xs y Nothing  = Just $ singleton xs y
        f xs y (Just t) = Just $ insert xs y t

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup []     (Trie y _) = y
lookup (x:xs) (Trie _ m) = Map.lookup x m >>= lookup xs

longestPrefix :: Ord a => [a] -> Trie a b -> Maybe ([a], b)
longestPrefix []     (Trie y _) = ([],) <$> y
longestPrefix (x:xs) (Trie y m) = (Map.lookup x m >>= longestPrefix xs) <|> fmap (x:xs,) y

inverseMap :: Ord b => Trie a b -> Map b [a]
inverseMap = Map.map reverse . flip (inverseMap' []) Map.empty
  where inverseMap' :: Ord b => [a] -> Trie a b -> Map b [a] -> Map b [a]
        inverseMap' xs (Trie (Just x) m2) m1 = inverseMap' xs (Trie Nothing m2) (Map.insert x xs m1)
        inverseMap' xs (Trie Nothing  m2) m1 = Map.foldrWithKey' (inverseMap' . (:xs)) m1 m2

