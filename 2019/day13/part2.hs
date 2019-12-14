{-# LANGUAGE GADTs, LambdaCase, MultiParamTypeClasses, RecordWildCards, TupleSections #-}

import IntCode (MonadicProgIO(..), runProgramM, parseProgram)

import Control.Arrow ((***), (&&&))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import UI.NCurses
import System.IO (hReady, hSetEcho, stdin)
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

instruction :: Window -> Integer -> Integer -> Integer -> Curses ()
instruction w (-1) 0 score = do
  updateWindow w $ do
    moveCursor 0 0
    clearLine
    drawString . show $ score
  render
instruction w x y tileId = do
  let tile = readTileId tileId
  updateWindow w $ do
    let glyph = flip Glyph [] $ case tile of
          Empty   -> ' '
          Wall    -> '\x2588'
          Block   -> '\x2592'
          HPaddle -> '\x2550'
          Ball    -> '\x25CF'
    moveCursor (y + 1) x
    drawGlyph glyph
  render
  case tile of
    Ball -> liftIO $ threadDelay 1000000
    _ -> return ()
  render

data InstructionReader
  = ReadX Window (Integer -> Integer -> Integer -> Curses ())
  | ReadY Window (Integer -> Integer -> Curses ())
  | ReadTileId Window (Integer -> Curses ())

readInstruction :: InstructionReader -> Integer -> (InstructionReader, Curses ())
readInstruction (ReadX w f) = (, return ()) . ReadY w . f
readInstruction (ReadY w f) = (, return ()) . ReadTileId w . f
readInstruction (ReadTileId w f) = (ReadX w (instruction w),) . f

data Game = Game
  { i :: InstructionReader
  , win :: Window
  }

instance MonadicProgIO Curses Game where
  getInputM Game{..} = fmap (, Game{..}) $ getEvent win (Just 0) >>= \case
    Just (EventSpecialKey KeyLeftArrow)  -> return (-1)
    Just (EventSpecialKey KeyRightArrow) -> return 1
    _                                    -> return 0
  putOutputM o Game{..} = do
    let (i', a) = readInstruction i o
    a
    return Game
      { i = i'
      , ..
      }

initialGame :: Window -> Game
initialGame w = Game
  { i = ReadX w (instruction w)
  , win = w
  }

{-# NOINLINE program #-}
program :: [Integer]
program = (2:) . tail . parseProgram . unsafePerformIO . readFile $ "input.txt"

waitForEnter :: Window -> Curses ()
waitForEnter w = getEvent w Nothing >>= \case
  Just (EventSpecialKey KeyEnter) -> return ()
  _ -> waitForEnter w

main :: IO ()
main = runCurses $ do
  w <- defaultWindow
  setCursorMode CursorInvisible
  runProgramM program (initialGame w)
  waitForEnter w

