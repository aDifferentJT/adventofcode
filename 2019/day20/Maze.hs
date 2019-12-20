{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}

module Maze (Maze, PortalType(..), Pos, addReached, findPortal, isPortal, isPortal', isUnreached, parseMaze) where

import Control.Arrow ((***), second)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)

data PortalType
  = Interior
  | Exterior
  deriving Show

data Square
  = Wall
  | Empty
  | EmptyReached
  | Portal { portalType :: PortalType, portalName :: String }
  | PortalReached { portalType :: PortalType, portalName :: String }
  deriving Show

parseSquare :: Char -> Square
parseSquare '#' = Wall
parseSquare '.' = Empty
parseSquare c = error $ "Unexpected square: " ++ show c

makeReached :: Square -> Square
makeReached Empty        = EmptyReached
makeReached (Portal t s) = PortalReached t s
makeReached sq           = error $ "Cannot make reached " ++ show sq

makePortal :: PortalType -> String -> Square -> Square
makePortal t s Empty = Portal t s
makePortal _ s sq    = error $ "Cannot make portal " ++ s ++ " in square " ++ show sq

isPortal :: Square -> Maybe String
isPortal (Portal _ s)        = Just s
isPortal (PortalReached _ s) = Just s
isPortal _                   = Nothing

isPortal' :: Square -> Maybe (PortalType, String)
isPortal' (Portal t s)        = Just (t, s)
isPortal' (PortalReached t s) = Just (t, s)
isPortal' _                   = Nothing

type Maze = Map Pos Square

isUnreachedSquare :: Square -> Bool
isUnreachedSquare Wall            = False
isUnreachedSquare Empty           = True
isUnreachedSquare EmptyReached    = False
isUnreachedSquare Portal{}        = True
isUnreachedSquare PortalReached{} = False

isUnreached :: Maze -> Pos -> Bool
isUnreached m p = maybe False isUnreachedSquare . Map.lookup p $ m

addReached :: Pos -> Maze -> Maze
addReached = Map.adjust makeReached

addPortal :: PortalType -> Pos -> String -> Maze -> Maze
addPortal t p s = Map.adjust (makePortal t s) p

findPortal :: String -> Maze -> [Pos]
findPortal s = Map.keys . Map.filter ((== Just s) . isPortal)

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal (x:xs) = head x : diagonal (map tail xs)

data Side = LeftSide | RightSide
data Dir = Horiz | Vert

parseMaze :: String -> Maze
parseMaze inputString = foldr (uncurry (uncurry . addPortal)) plainMaze portals
  where ls :: [String]
        ls = filter (not . null) . lines $ inputString
        margin :: Int
        margin = length . takeWhile (== ' ') . diagonal $ ls
        thickness :: Int
        thickness = length . takeWhile (/= ' ') . drop margin . diagonal $ ls
        holeX :: Int
        holeX = length (head ls) - 2 * (margin + thickness)
        holeY :: Int
        holeY = length ls - 2 * (margin + thickness)
        plainMaze :: Maze
        plainMaze =
          Map.fromList
          . map (second parseSquare)
          . uncurry (++)
          . (   concatMap
                  ( take (2 * thickness + holeX)
                  . drop margin
                  )
            *** ( uncurry (++)
                . (   concatMap
                        ( uncurry (++)
                        . second
                            ( take thickness
                            . drop holeX
                            )
                        . splitAt thickness
                        . drop margin
                        )
                  *** concatMap
                        ( take (2 * thickness + holeX)
                        . drop margin
                        )
                    . take thickness
                  )
                . splitAt holeY
                )
            )
          . splitAt thickness
          . drop margin
          . zipWith
              (\y -> zip (map (,y) [0..]))
              [0..]
          $ ls
        portalsAt :: Int -> Side -> Dir -> [(Pos, String)]
        portalsAt pos side dir =
          filter (all (\c -> c >= 'A' && c <= 'Z') . snd)
          . zip
              ( map
                  ( ( case dir of
                        Horiz -> id
                        Vert  -> flip
                    ) (,)
                      ( case side of
                          LeftSide  -> pos
                          RightSide -> pos - 1
                      )
                  )
                  [0..]
              )
          . map
              ( take margin
              . drop
                  ( case side of
                      LeftSide  -> pos - margin
                      RightSide -> pos
                  )
              )
          . ( case dir of
                Horiz -> id
                Vert  -> transpose
            )
          $ ls
        portals :: [(PortalType, (Pos, String))]
        portals = concat
          [ map (Exterior,) $ portalsAt margin LeftSide Horiz
          , map (Exterior,) $ portalsAt margin LeftSide Vert
          , map (Exterior,) $ portalsAt (margin + 2 * thickness + holeX) RightSide Horiz
          , map (Exterior,) $ portalsAt (margin + 2 * thickness + holeY) RightSide Vert
          , map (Interior,) $ portalsAt (margin + thickness) RightSide Horiz
          , map (Interior,) $ portalsAt (margin + thickness) RightSide Vert
          , map (Interior,) $ portalsAt (margin + thickness + holeX) LeftSide Horiz
          , map (Interior,) $ portalsAt (margin + thickness + holeY) LeftSide Vert
          ]

