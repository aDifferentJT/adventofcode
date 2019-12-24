{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes #-}

import Data.Bits ((.&.), shiftR)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli.Class (KnownNat, Mod, (^%), getVal, invertMod)
import Text.Parsec (ParseError, choice, endBy, many1, try)
import Text.Parsec.Char (digit, newline, space, string)
import Text.Parsec.String (Parser, parseFromFile)

data Func m
  = Add (Mod m)
  | Mul (Mod m)
  deriving (Eq, Show)

applyFunc :: KnownNat m => Mod m -> Func m -> Mod m
applyFunc x (Add y) = x + y
applyFunc x (Mul y) = x * y

applyFuncs :: KnownNat m => Mod m -> [Func m] -> Mod m
applyFuncs = foldl' applyFunc

simplify :: KnownNat m => [Func m] -> [Func m]
simplify = simplify' True
  where simplify' :: KnownNat m => Bool -> [Func m] -> [Func m]
        simplify' _ (Add x : Add y : fs) = simplify $ Add (x + y) : fs
        simplify' _ (Mul x : Mul y : fs) = simplify $ Mul (x * y) : fs
        simplify' _ (Add x : Mul y : fs) = simplify $ Mul y : Add (x * y) : fs
        simplify' True (Mul x : fs@(Add _ : _)) = simplify' False $ Mul x : simplify fs
        simplify' _ fs = fs

invertFuncs :: KnownNat m => [Func m] -> [Func m]
invertFuncs = simplify . reverse . map invert
  where invert :: KnownNat m => Func m -> Func m
        invert (Add n) = Add (-n)
        invert (Mul n) = Mul . fromJust . invertMod $ n

replicateFuncs :: KnownNat m => Integer -> [Func m] -> [Func m]
replicateFuncs 0 _ = []
replicateFuncs n fs = simplify . concat $
  [ if n .&. 1 == 1 then fs else []
  , concat . replicate 2 $ replicateFuncs (n `shiftR` 1) fs
  ]

dealIntoNewStack :: KnownNat m => [Func m]
dealIntoNewStack = [Add 1, Mul (-1)]

cut :: KnownNat m => Mod m -> [Func m]
cut n = [Add (-n)]

dealWithIncrement :: Mod m -> [Func m]
dealWithIncrement n = [Mul n]

parseInteger :: KnownNat m => Parser (Mod m)
parseInteger = do
  s <- choice [string "-", return ""]
  ds <- many1 digit
  return . fromInteger . read $ s ++ ds

parseTechnique :: KnownNat m => Parser [Func m]
parseTechnique = simplify . concat <$> endBy parseTechnique' newline
  where parseTechnique' :: KnownNat m => Parser [Func m]
        parseTechnique' = choice
          [ try $ do
              string "deal into new stack"
              return dealIntoNewStack
          , try $ do
              string "cut"
              space
              cut <$> parseInteger
          , try $ do
              string "deal with increment"
              space
              dealWithIncrement <$> parseInteger
          ]

main :: IO ()
main = do
  let fs () = either (error . show) id <$> parseFromFile parseTechnique "input.txt"
  applyFuncs 2019 <$> (fs () :: IO [Func 10007]) >>= print
  applyFuncs 2020 . replicateFuncs 101741582076661 . invertFuncs <$> (fs () :: IO [Func 119315717514047]) >>= print

