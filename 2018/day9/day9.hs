{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}

numPlayers :: Int
numPlayers = 468

lastMarble :: Int
lastMarble = 7101000

data Circle = Circle
  { circleFront :: [Int]
  , circleBack :: [Int]
  , circleLen :: Int
  , marbleQueue :: [Int]
  , playerScores :: [Int]
  , player :: Int
  }

initialCircle :: Circle
initialCircle = Circle
  { circleFront = [0]
  , circleBack = []
  , circleLen = 1
  , marbleQueue = [1..lastMarble]
  , playerScores = replicate numPlayers 0
  , player = 0
  }

adjustValue :: Int -> (a -> a) -> [a] -> [a]
adjustValue 0 f (x:xs) = f x : xs
adjustValue n f (x:xs) = x : adjustValue (n - 1) f xs

moveCircleBy :: Int -> Circle -> Circle
moveCircleBy n Circle{..}
  | n == 0 = Circle{..}
  | n <= -circleLen = moveCircleBy (n + circleLen) Circle{..}
  | n >= circleLen = moveCircleBy (n - circleLen) Circle{..}
  | n < 0 = case circleBack of
    [] -> moveCircleBy n $ Circle
      { circleFront = []
      , circleBack = reverse circleFront
      , ..
      }
    x:xs -> moveCircleBy (n + 1) $ Circle
      { circleFront = x : circleFront
      , circleBack = xs
      , ..
      }
  | n > 0 = case circleFront of
    [] -> moveCircleBy n $ Circle
      { circleFront = reverse circleBack
      , circleBack = []
      , ..
      }
    x:xs -> moveCircleBy (n - 1) $ Circle
      { circleFront = xs
      , circleBack = x : circleBack
      , ..
      }

makeMove :: Circle -> Either [Int] Circle
makeMove Circle{ marbleQueue=[], playerScores } = Left playerScores
makeMove c@Circle{marbleQueue}
  | head marbleQueue `mod` 23 == 0 = Right $
    let Circle{..} = moveCircleBy (-7) c in
    Circle
      { circleFront = tail circleFront
      , circleLen = circleLen - 1
      , marbleQueue = tail marbleQueue
      , playerScores = adjustValue player (+ (head marbleQueue + head circleFront)) playerScores
      , player = (player + 1) `mod` numPlayers
      , ..
      }
  | otherwise = Right $
    let Circle{..} = moveCircleBy 2 c in
    Circle
      { circleFront = head marbleQueue : circleFront
      , circleLen = circleLen + 1
      , marbleQueue = tail marbleQueue
      , player = (player + 1) `mod` numPlayers
      , ..
      }

playGame :: Circle -> [Int]
playGame (makeMove -> Left scores) = scores
playGame (makeMove -> Right c) = playGame c

main :: IO ()
main = print . maximum . playGame $ initialCircle

