{-# LANGUAGE LambdaCase, MultiParamTypeClasses, NamedFieldPuns, RecordWildCards, TupleSections #-}

import IntCode (MonadicProgIO(..), parseProgram, runProgramM)

import Prelude hiding (id)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( atomically
  , TQueue, newTQueue, readTQueue, tryReadTQueue, writeTQueue
  , TVar, newTVar, readTVarIO, modifyTVar
  )
import Control.Monad (void)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data OutputQueue
  = NoOutput
  | Dest Integer
  | DestX Integer Integer

data Computer = Computer
  { inputQueue :: Maybe Integer
  , outputQueue :: OutputQueue
  , queues :: Map Integer (TQueue (Integer, Integer))
  , idles :: TVar (Set Integer)
  , id :: Integer
  }

makeComputer :: Map Integer (TQueue (Integer, Integer)) -> TVar (Set Integer) -> Integer -> Computer
makeComputer queues idles id = Computer{..}
  where inputQueue = Just id
        outputQueue = NoOutput

setIdle :: Bool -> Integer -> TVar (Set Integer) -> IO ()
setIdle True  id idles = atomically . modifyTVar idles . Set.insert $ id
setIdle False id idles = atomically . modifyTVar idles . Set.delete $ id

instance MonadicProgIO IO Computer where
  getInputM c@Computer{ inputQueue = Just y } = return (Just y, c { inputQueue = Nothing })
  getInputM c@Computer{ inputQueue = Nothing, .. } = (atomically . tryReadTQueue $ queues ! id) >>= \case
    Nothing     -> setIdle True id idles >> return (Just (-1), c)
    Just (x, y) -> return (Just x, c { inputQueue = Just y })
  putOutputM d c@Computer{ outputQueue = NoOutput, .. } = do
    setIdle False id idles
    return (True, c { outputQueue = Dest d })
  putOutputM x c@Computer{ outputQueue = Dest d } =
    return (True, c { outputQueue = DestX d x })
  putOutputM y c@Computer{ outputQueue = DestX d x, .. } = do
    atomically . writeTQueue (queues ! d) $ (x, y)
    setIdle False d idles
    return (True, c { outputQueue = NoOutput })

makeQueues :: IO (Map Integer (TQueue (Integer, Integer)))
makeQueues = atomically $ Map.fromList <$> mapM ((<$> newTQueue) . (,)) [0..49]

runComputer :: [Integer] -> Map Integer (TQueue (Integer, Integer)) -> TVar (Set Integer) -> Integer -> IO ()
runComputer program queues idles = void . runProgramM program . makeComputer queues idles

runComputers :: [Integer] -> Map Integer (TQueue (Integer, Integer)) -> TVar (Set Integer) -> IO ()
runComputers program queues idles = mapM_ (forkIO . runComputer program queues idles) $ [0..49]

nat :: TQueue Integer -> Map Integer (TQueue (Integer, Integer)) -> TVar (Set Integer) -> (Integer, Integer) -> IO ()
nat outY queues idles p = (atomically . tryReadTQueue $ queues ! 255) >>= \case
  Just p' -> nat outY queues idles p'
  Nothing -> Set.size <$> readTVarIO idles >>= \case
    50 -> do
      atomically . writeTQueue outY . snd $ p
      atomically . writeTQueue (queues ! 0) $ p
      setIdle False 0 idles
      nat outY queues idles p
    _  -> nat outY queues idles p

getFirstDup :: Eq a => TQueue a -> IO a
getFirstDup q = (atomically . readTQueue $ q) >>= getFirstDup' q
  where getFirstDup' :: Eq a => TQueue a -> a -> IO a
        getFirstDup' q x = (atomically . readTQueue $ q) >>= \y -> if x == y then return y else getFirstDup' q y

main :: IO ()
main = do
  program <- parseProgram <$> readFile "input.txt"
  queues <- Map.insert 255 <$> atomically newTQueue <*> makeQueues
  idles <- atomically . newTVar $ Set.empty
  runComputers program queues idles
  firstNatPacket <- atomically . readTQueue $ queues ! 255
  print firstNatPacket
  outY <- atomically newTQueue
  forkIO $ nat outY queues idles firstNatPacket
  getFirstDup outY >>= print

