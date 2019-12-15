{-# LANGUAGE GADTs, LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, RecordWildCards, TemplateHaskell, TupleSections #-}

import IntCode (MonadicProgIO(..), runProgramM, parseProgram)

import Control.Arrow ((***), (&&&), first)
import Control.Concurrent (threadDelay)
import Control.Lens (makeLenses)
import qualified Control.Lens as Lens
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

data InstructionReader
  = ReadX Window (Integer -> Integer -> Integer -> Game -> (Game, Curses ()))
  | ReadY Window (Integer -> Integer -> Game -> (Game, Curses ()))
  | ReadTileId Window (Integer -> Game -> (Game, Curses ()))

data Game = Game
  { _i :: InstructionReader
  , win :: Window
  , autoplay :: Bool
  , ballX :: Integer
  , paddleX :: Integer
  , yourScore :: Integer
  , frameTime :: Int
  }

makeLenses ''Game

instruction :: Window -> Integer -> Integer -> Integer -> Game -> (Game, Curses ())
instruction w (-1) 0 score g =
  ( g { yourScore = score }
  , do
      updateWindow w $ do
        moveCursor 0 0
        clearLine
        drawString . show $ score
      render
  )
instruction w x y tileId g =
  let tile = readTileId tileId in
  ( case tile of
      Ball    -> g { ballX = x }
      HPaddle -> g { paddleX = x }
      _       -> g
  , do
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
  )

readInstruction
  :: InstructionReader
  -> Integer
  -> Game
  -> (Game, Curses ())
readInstruction (ReadX w f) = ((, return ()) .) . Lens.set i . ReadY w . f
readInstruction (ReadY w f) = ((, return ()) .) . Lens.set i . ReadTileId w . f
readInstruction (ReadTileId w f) = (first (Lens.set i (ReadX w (instruction w))) .) . f

instance MonadicProgIO Curses Game where
  getInputM g@Game{ autoplay = False, .. } = do
    liftIO . threadDelay $ frameTime
    key <- getEvent win (Just 0)
    case key of
      Just (EventSpecialKey KeyLeftArrow)  -> return (-1, g)
      Just (EventSpecialKey KeyRightArrow) -> return (1, g)
      _                                    -> return (0, g)
  getInputM g@Game{ autoplay = True, .. } = do
    liftIO . threadDelay $ frameTime
    if | ballX <  paddleX -> return (-1, g)
       | ballX == paddleX -> return (0, g)
       | ballX >  paddleX -> return (1, g)
  putOutputM o g@Game{..} = do
    let (g', a) = readInstruction _i o g
    a
    return g'

initialGame :: Window -> Bool -> Int -> Game
initialGame win autoplay fps = Game
  { _i = ReadX win (instruction win)
  , ballX = 0
  , paddleX = 0
  , yourScore = 0
  , frameTime = 1000000 `div` fps
  , ..
  }

{-# NOINLINE program #-}
program :: [Integer]
program = (2:) . tail . parseProgram . unsafePerformIO . readFile $ "input.txt"

waitForEnter :: Window -> Curses ()
waitForEnter w = getEvent w Nothing >>= \case
  Just (EventSpecialKey KeyEnter) -> return ()
  _ -> waitForEnter w

main :: IO ()
main = do
  putStrLn "Press 1<Enter> if you want to play, 2<Enter> if you want auto-play"
  autoplay <- (== "2") <$> getLine
  putStrLn "Enter the desired fps"
  fps <- readLn
  score <- runCurses $ do
    w <- defaultWindow
    setCursorMode CursorInvisible
    Game{yourScore} <- runProgramM program (initialGame w autoplay fps)
    closeWindow w
    return yourScore
  putStrLn $ "Score: " ++ show score

