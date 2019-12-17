{-# LANGUAGE LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, RecordWildCards #-}

import IntCode (MonadicProgIO(..), runProgramM, parseProgram)

import Control.Arrow ((***), (&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import UI.NCurses
import System.IO.Unsafe (unsafePerformIO)

data Tile
  = Wall
  | Empty
  | Oxygen
  deriving Enum

readTileId :: Integer -> Tile
readTileId = toEnum . fromInteger

data Direction
  = North
  | South
  | West
  | East

moveIn :: Direction -> (Int, Int) -> (Int, Int)
moveIn North (x, y) = (x, y - 1)
moveIn South (x, y) = (x, y + 1)
moveIn West  (x, y) = (x - 1, y)
moveIn East  (x, y) = (x + 1, y)

data Droid = Droid
  { win :: Window
  , p :: (Int, Int)
  , m :: Map (Int, Int) Tile
  , dir :: Direction
  }

showMap :: Droid -> String
showMap Droid{..} = unlines
    [ [ if | (x, y) == (0, 0) -> 'I'
           | (x, y) == p      -> 'D'
           | otherwise ->
             case Map.lookup (x, y) m of
               Just Wall   -> '\x2588'
               Just Empty  -> ' '
               Just Oxygen -> 'O'
               _           -> '?'
        | x <- [l-1 .. r+1]
      ]
      | y <- [t-1 .. b+1]
    ]
  where ((l, r), (t, b)) =
          ((minimum &&& maximum) *** (minimum &&& maximum))
          . (map fst &&& map snd)
          . Map.keys
          $ m


instance MonadicProgIO Curses Droid where
  getInputM d@Droid{win} = do
    key <- getEvent win Nothing
    case key of
      Just (EventSpecialKey KeyUpArrow)    -> return (Just 1, d { dir = North })
      Just (EventSpecialKey KeyDownArrow)  -> return (Just 2, d { dir = South })
      Just (EventSpecialKey KeyLeftArrow)  -> return (Just 3, d { dir = West })
      Just (EventSpecialKey KeyRightArrow) -> return (Just 4, d { dir = East })
  putOutputM o d = do
    let p' = moveIn (dir d) (p d)
    let t = readTileId o
    let d' = case t of
          Wall   -> d
          Empty  -> d { p = p' }
          Oxygen -> d { p = p' }
    let d'' = d' { m = Map.insert p' t . m $ d' }

    updateWindow (win d) $ do
      clear
      moveCursor 0 0
      drawString . showMap $ d''
    render

    return (True, d'')

initialDroid :: Window -> Droid
initialDroid win = Droid
  { p = (0, 0)
  , m = Map.singleton (0, 0) Empty
  , dir = North
  , ..
  }

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

waitForEnter :: Window -> Curses ()
waitForEnter w = getEvent w Nothing >>= \case
  Just (EventSpecialKey KeyEnter) -> return ()
  _ -> waitForEnter w

main :: IO ()
main =
  runCurses $ do
    w <- defaultWindow
    setCursorMode CursorInvisible
    runProgramM program (initialDroid w)
    closeWindow w

