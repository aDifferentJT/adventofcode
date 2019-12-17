{-# LANGUAGE GADTs, LambdaCase, RecordWildCards, TupleSections #-}

import IntCode (ProgIO(..), runProgram, parseProgram)

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

data TileType
  = Empty
  | Wall
  | Block
  | HPaddle
  | Ball
  deriving Enum

readTileId :: Integer -> TileType
readTileId = toEnum . fromInteger

type Screen = Map (Integer, Integer) TileType

instruction :: Integer -> Integer -> Integer -> Screen -> Screen
instruction x y = Map.insert (x, y) . readTileId

data InstructionReader
  = ReadX (Integer -> Integer -> Integer -> Screen -> Screen)
  | ReadY (Integer -> Integer -> Screen -> Screen)
  | ReadTileId (Integer -> Screen -> Screen)

readInstruction :: InstructionReader -> Integer -> Screen -> (InstructionReader, Screen)
readInstruction (ReadX f) = (,) . ReadY . f
readInstruction (ReadY f) = (,) . ReadTileId . f
readInstruction (ReadTileId f) = ((ReadX instruction,) .) . f

data Game = Game
  { i :: InstructionReader
  , s :: Screen
  }

instance ProgIO Game where
  getInput = (Just 0,)
  putOutput o Game{..} = (True, uncurry Game . readInstruction i o $ s)

initialGame :: Game
initialGame = Game
  { i = ReadX instruction
  , s = Map.empty
  }

countBlocks :: Game -> Int
countBlocks = Map.size . Map.filter (\case Block -> True; _ -> False) . s

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = print . countBlocks . runProgram program $ initialGame

