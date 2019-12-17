{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}

import IntCode (ProgIO(..), runProgram, parseProgram)

import Control.Applicative ((<|>))
import Control.Arrow ((***), (&&&), first)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

data Direction
  = North
  | South
  | West
  | East
  deriving (Enum, Show)

type Path = [Direction]

data Tree
  = Wall
  | Empty { n :: Tree, s :: Tree, w :: Tree, e :: Tree }
  | Backtrack
  | Unexplored
  deriving Show

getSubtreeAt :: Tree -> Path -> Tree
getSubtreeAt = foldr f
  where f :: Direction -> Tree -> Tree
        f North (Empty t _ _ _) = t
        f South (Empty _ t _ _) = t
        f West  (Empty _ _ t _) = t
        f East  (Empty _ _ _ t) = t

setSubtreeAt :: Tree -> Path -> Tree -> Tree
setSubtreeAt t = setSubtreeAt' t . reverse
  where setSubtreeAt' :: Tree -> Path -> Tree -> Tree
        setSubtreeAt' _ [] t = t
        setSubtreeAt' Empty{..} (North:p) t = Empty { n = setSubtreeAt' n p t, .. }
        setSubtreeAt' Empty{..} (South:p) t = Empty { s = setSubtreeAt' s p t, .. }
        setSubtreeAt' Empty{..} (West:p)  t = Empty { w = setSubtreeAt' w p t, .. }
        setSubtreeAt' Empty{..} (East:p)  t = Empty { e = setSubtreeAt' e p t, .. }

rerootTree :: Tree -> Path -> Tree
rerootTree = foldr f
  where f :: Direction -> Tree -> Tree
        f North t = (n t) { s = t { n = Backtrack } }
        f South t = (s t) { n = t { s = Backtrack } }
        f West  t = (w t) { e = t { w = Backtrack } }
        f East  t = (e t) { w = t { e = Backtrack } }

depth :: Tree -> Integer
depth Wall       = -1
depth Empty{..}  = (+1) . maximum . map depth $ [n,s,w,e]
depth Backtrack  = -1
depth Unexplored = -1

unexploredDir :: Tree -> Maybe Direction
unexploredDir Empty{ n = Unexplored } = Just North
unexploredDir Empty{ s = Unexplored } = Just South
unexploredDir Empty{ w = Unexplored } = Just West
unexploredDir Empty{ e = Unexplored } = Just East
unexploredDir Empty{} = Nothing

backtrackDir :: Tree -> Maybe Direction
backtrackDir Empty{ n = Backtrack } = Just North
backtrackDir Empty{ s = Backtrack } = Just South
backtrackDir Empty{ w = Backtrack } = Just West
backtrackDir Empty{ e = Backtrack } = Just East
backtrackDir Empty{} = Nothing

data Droid = Droid
  { tree :: Tree
  , move :: Maybe Direction
  , path :: Path
  , subtree :: Tree 
  , pathToO2 :: Maybe Path
  }
  deriving Show

instance ProgIO Droid where
  getInput Droid{ move = _, .. } =
    let move = unexploredDir subtree in
    ( (+1) . fromIntegral . fromEnum <$> (move <|> backtrackDir subtree)
    , Droid{..}
    )

  putOutput 1 Droid{ move = Nothing, path = _:path, .. } =
    ( True
    , Droid
      { move = Nothing
      , subtree = getSubtreeAt tree path
      , ..
      }
    )
  putOutput 0 Droid{ move = Just dir, .. } =
    ( True
    , let subtree' = case dir of
            North -> subtree { n = Wall }
            South -> subtree { s = Wall }
            West  -> subtree { w = Wall }
            East  -> subtree { e = Wall }
      in
      Droid
        { tree = setSubtreeAt tree path subtree'
        , move = Nothing
        , subtree = subtree'
        , ..
        }
    )
  putOutput o@(flip elem [1,2] -> True) Droid{ move = Just dir, .. } =
    ( True
    , let path' = dir : path in
      let subtree' = case dir of
            North -> Empty Unexplored Backtrack Unexplored Unexplored
            South -> Empty Backtrack Unexplored Unexplored Unexplored
            West  -> Empty Unexplored Unexplored Unexplored Backtrack
            East  -> Empty Unexplored Unexplored Backtrack Unexplored
      in
      Droid
        { tree = setSubtreeAt tree path' subtree'
        , move = Nothing
        , path = path'
        , subtree = subtree'
        , pathToO2 = if o == 2 then Just path' else pathToO2
        }
    )
  putOutput n d = error . show $ (n, d)

initialDroid :: Droid
initialDroid = Droid
  { tree = Unexplored
  , move = Nothing
  , path = []
  , subtree = Empty Unexplored Unexplored Unexplored Unexplored
  , pathToO2 = Nothing
  }

{-# NOINLINE program #-}
program :: [Integer]
program = parseProgram . unsafePerformIO . readFile $ "input.txt"

main :: IO ()
main = do
  let Droid{..} = runProgram program initialDroid
  print . fmap length $ pathToO2
  let o2Tree = rerootTree tree <$> pathToO2
  print . fmap depth $ o2Tree

