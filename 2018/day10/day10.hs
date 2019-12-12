{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

import Control.Monad (unless, void)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec

type Vec2 = (Int, Int)

instance Num Vec2 where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (*) = undefined
  abs = undefined
  signum (x, y) = (signum x, signum y)
  fromInteger 0 = (0, 0)
  fromInteger _ = undefined
  negate (x, y) = (-x, -y)

data Star = Star
  { p :: Vec2
  , v :: Vec2
  }
  deriving (Eq, Ord, Show)

pchar :: Char -> ReadPrec ()
pchar c = do
  c' <- get
  unless (c == c') pfail

pstring :: String -> ReadPrec ()
pstring = void . traverse pchar

instance Read Star where
  readPrec = do
    pstring "position=<"
    x <- readPrec
    pstring ", "
    y <- readPrec
    pstring "> velocity=<"
    vx <- readPrec
    pstring ", "
    vy <- readPrec
    pstring ">"
    return Star { p = (x, y), v = (vx, vy) }

applyVelocity :: [Star] -> [Star]
applyVelocity = map f
  where f :: Star -> Star
        f Star{ p, v } = Star { p = p + v, v }

showStars :: [Star] -> String
showStars ss =
  let l = minimum . map (fst . p) $ ss in
  let r = maximum . map (fst . p) $ ss in
  let t = minimum . map (snd . p) $ ss in
  let b = maximum . map (snd . p) $ ss in
  unlines
    [ [ if any ((== (x, y)) . p) ss then '\x2588' else ' '
        | x <- [l..r]
      ]
      | y <- [t..b]
    ]

startConditions :: IO [Star]
startConditions = map read . lines <$> readFile "input.txt"

runStars :: [Star] -> IO ()
runStars ss = do
  putStrLn . showStars $ ss
  getChar
  runStars . applyVelocity $ ss

main :: IO ()
main = startConditions >>= runStars

