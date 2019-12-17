{-# LANGUAGE RecordWildCards #-}

import IntCode (ProgIO(..), runProgram, parseProgram)

import Control.Arrow ((***), (&&&), first, second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

data Direction
  = DirUp
  | DirRight
  | DirDown
  | DirLeft

rotateLeft :: Direction -> Direction
rotateLeft DirUp    = DirLeft
rotateLeft DirRight = DirUp
rotateLeft DirDown  = DirRight
rotateLeft DirLeft  = DirDown

rotateRight :: Direction -> Direction
rotateRight DirUp    = DirRight
rotateRight DirRight = DirDown
rotateRight DirDown  = DirLeft
rotateRight DirLeft  = DirUp

data Instruction
  = Paint
  | Rotate

data Robot = Robot
  { position :: (Integer, Integer)
  , orientation :: Direction
  , panels :: Map (Integer, Integer) Integer
  , nextInstruction :: Instruction
  }

initRobot :: Robot
initRobot = Robot
  { position = (0, 0)
  , orientation = DirUp
  , panels = Map.empty
  , nextInstruction = Paint
  }

initRobotWhite :: Robot
initRobotWhite = initRobot
  { panels = Map.singleton (0, 0) 1
  }

moveForward :: Robot -> Robot
moveForward r@Robot{ orientation = DirUp,    .. } = r { position = second (+1) position }
moveForward r@Robot{ orientation = DirRight, .. } = r { position = first (+1) position }
moveForward r@Robot{ orientation = DirDown,  .. } = r { position = second (subtract 1) position }
moveForward r@Robot{ orientation = DirLeft,  .. } = r { position = first (subtract 1) position }

instance ProgIO Robot where
  getInput Robot{..} = (Just . fromMaybe 0 . Map.lookup position $ panels, Robot{..})

  putOutput colour Robot{ nextInstruction = Paint, .. } =
    ( True
    , Robot
      { panels = Map.insert position colour panels
      , nextInstruction = Rotate
      , ..
      }
    )
  putOutput 0 Robot{ nextInstruction = Rotate, .. } =
    ( True
    , moveForward $ Robot
      { orientation = rotateLeft orientation
      , nextInstruction = Paint
      , ..
      }
    )
  putOutput 1 Robot{ nextInstruction = Rotate, .. } =
    ( True
    , moveForward $ Robot
      { orientation = rotateRight orientation
      , nextInstruction = Paint
      , ..
      }
    )

countPaintedPanels :: Robot -> Int
countPaintedPanels = Map.size . panels

showPanels :: Robot -> String
showPanels Robot{..} = unlines
    [ [ case Map.lookup (x, y) panels of
          Just 1 -> '\x2588'
          _      -> ' '
        | x <- [l..r]
      ]
      | y <- reverse [t..b]
    ]
  where ((l, r), (t, b)) =
          ((minimum &&& maximum) *** (minimum &&& maximum))
          . (map fst &&& map snd)
          . Map.keys
          $ panels

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

numPaintedPanels :: Int
numPaintedPanels = countPaintedPanels . runProgram program $ initRobot

showPanelsStartingWhite :: String
showPanelsStartingWhite = showPanels . runProgram program $ initRobotWhite

main :: IO ()
main = putStrLn showPanelsStartingWhite

