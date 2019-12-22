{-# LANGUAGE LambdaCase, MultiParamTypeClasses, TupleSections #-}

import IntCode (MonadicProgIO(..), parseProgram, runProgramM)

import Control.Arrow ((&&&), (***))
import Data.Char (chr, ord)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

data SpringDroid = SpringDroid { damage :: Maybe Integer, program :: String }

pushMaybeIntoPair :: Maybe (a, b) -> (Maybe a, Maybe b)
pushMaybeIntoPair = maybe (Nothing, Nothing) (Just *** Just)

instance MonadicProgIO IO SpringDroid where
  getInputM =
    uncurry (flip (fmap . flip (,)))
    . (   uncurry (>>) . (maybe (return ()) putChar &&& return . fmap (fromIntegral . ord))
      *** SpringDroid Nothing . fromMaybe []
      )
    . pushMaybeIntoPair
    . uncons
    . program
  putOutputM o d
    | o < 128   = (putChar . chr . fromInteger $ o) >> return (True, d)
    | otherwise = return (False, d { damage = Just o })

runSpringDroid :: FilePath -> IO (Maybe Integer)
runSpringDroid springScriptFn = do
  intCodeProgram <- parseProgram <$> readFile "input.txt"
  springScriptProgram <- unlines . filter (not . null) . lines <$> readFile springScriptFn
  fmap damage . runProgramM intCodeProgram . SpringDroid Nothing $ springScriptProgram

main :: IO ()
main = do
  fn <- head <$> getArgs
  runSpringDroid fn >>= \case
    Just damage -> putStrLn $ "Damage: " ++ show damage
    Nothing     -> return ()
