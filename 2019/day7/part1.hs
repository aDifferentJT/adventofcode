{-# LANGUAGE TupleSections #-}

import IntCode (parseProgram, runProgramLists)

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE program #-}
program :: [Integer]
program = unsafePerformIO $ parseProgram <$> readFile "input.txt"

{-# NOINLINE cache #-}
cache :: IORef (Map Integer (Integer, Integer))
cache = unsafePerformIO . newIORef $ Map.empty

maximumWith :: Ord o => (a -> o) -> [a] -> a
maximumWith f = fst . maximumWith' f
  where maximumWith' :: Ord o => (a -> o) -> [a] -> (a, o)
        maximumWith' f [x]    = (x, f x)
        maximumWith' f (x:xs) =
          let fx = f x in
          let (y, fy) = maximumWith' f xs in
          if fx >= fy
          then (x, fx)
          else (y, fy)

bestPhaseForInput :: ([Integer] -> Integer -> IO Integer) -> [Integer] -> Integer -> IO (Integer, Integer)
bestPhaseForInput f ps i = maybe bestPhaseForInput' return . Map.lookup i =<< readIORef cache
  where bestPhaseForInput' :: IO (Integer, Integer)
        bestPhaseForInput' = do
          p <- maximumWith snd <$> mapM tryPhase ps
          modifyIORef cache (Map.insert i p)
          return p
        tryPhase :: Integer -> IO (Integer, Integer)
        tryPhase p = fmap (p,) . f (filter (/= p) ps) . head . runProgramLists program $ [p, i]

bestPhaseForInputN :: Integer -> [Integer] -> Integer -> IO (Integer, Integer)
bestPhaseForInputN 0 _  = return . (0,)
bestPhaseForInputN n ps = bestPhaseForInput ((fmap snd .) . bestPhaseForInputN (n - 1)) ps

main :: IO ()
main = snd <$> bestPhaseForInputN 5 [0..4] 0 >>= print

