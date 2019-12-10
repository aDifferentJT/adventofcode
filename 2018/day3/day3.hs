{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless, void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadPrec
import Text.Read (readPrec)

import Debug.Trace (trace)

data Claim = Claim
  { iden :: Int
  , l :: Int
  , t :: Int
  , w :: Int
  , h :: Int
  }
  deriving (Eq, Ord, Show)

pchar :: Char -> ReadPrec ()
pchar c = do
  c' <- get
  unless (c == c') pfail

pstring :: String -> ReadPrec ()
pstring = void . traverse pchar

instance Read Claim where
  readPrec = do
    pstring "#"
    iden <- readPrec
    pstring " @ "
    l <- readPrec
    pstring ","
    t <- readPrec
    pstring ": "
    w <- readPrec
    pstring "x"
    h <- readPrec
    return Claim{..}

squaresInClaim :: Claim -> [(Int, Int)]
squaresInClaim Claim{..} = [(x,y) | x <- [l..l+w-1], y <- [t..t+h-1]]

claimArea :: [Claim] -> Map (Int, Int) (Set Claim)
claimArea = foldr f Map.empty
  where f :: Claim -> Map (Int, Int) (Set Claim) -> Map (Int, Int) (Set Claim)
        f c m = foldr (Map.alter (Just . maybe (Set.singleton c) (Set.insert c))) m . squaresInClaim $ c

main :: IO ()
main = do
  cs <- map (read :: String -> Claim) . lines <$> readFile "input.txt"
  let overlappingClaims = filter ((>1) . Set.size) . Map.elems . claimArea $ cs
  print . length $ overlappingClaims
  print (Set.difference (Set.fromList cs) (Set.unions overlappingClaims))

