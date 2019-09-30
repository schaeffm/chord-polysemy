{-# LANGUAGE DuplicateRecordFields #-}

module Routing (Routing, ownAddress, successor, predecessor, fingers, runRouting, closestPrecedingNode, setPredecessor, setFinger, showRouting, routingToReader, setSuccessors, setSuccessor, successors) where
import Polysemy
import Polysemy.Reader
import Control.Concurrent.STM
import Data.List (find)

import RoutingTable (RoutingTable, createRoutingTable)
import Identifier
import Util

import qualified RoutingTable as Self

data Routing addr m a where
  SetPredecessor :: Maybe addr -> Routing addr m ()
  SetSuccessors :: [addr] -> Routing addr m ()
  SetSuccessor :: addr -> Routing addr m ()
  SetFinger :: Int -> addr -> Routing addr m ()
  ClosestPrecedingNode :: Identify i => i -> Routing addr m addr
  OwnAddress :: Routing addr m addr
  Successor :: Routing addr m addr
  Successors :: Routing addr m [addr]
  Predecessor :: Routing addr m (Maybe addr)
  Fingers :: Routing addr m Int
  FingerTable :: Routing addr m [addr]
makeSem ''Routing

showRouting :: (Show addr, Member (Routing addr) r) => Sem r String
showRouting = do
  s <- successor
  p <- predecessor
  fs <- fingerTable
  return $
    "Successor: " ++ show s ++ "\n" ++
    "Predecessor: " ++ show p ++ "\n" ++ 
    "Fingers: " ++ show fs

fingerCount :: Int
fingerCount = 4

succCount :: Int
succCount = 4

routingToReader ::
     (Member (Embed IO) r, Identify addr)
  => Sem (Routing addr:r) a
  -> Sem (Reader (TVar (RoutingTable addr)) : r) a
routingToReader =
  reinterpret $ \case
    SetPredecessor new -> do
      peer <- ask
      embed (modifyTVarIO peer (\p -> p {Self.predecessor = new}))
    SetSuccessor new -> do
        peer <- ask
        n <- Self.address <$> getPeer
        succsOld <- Self.successors <$> getPeer
        let succsNew = take succCount $ sortIdList n (new:succsOld)
        embed $ modifyTVarIO peer (\p -> p {Self.successors = succsNew})
    SetSuccessors new -> do
      peer <- ask
      n <- Self.address <$> getPeer
      embed $ modifyTVarIO peer (\p -> p {Self.successors = sortIdList n new})
    SetFinger i new -> do
      peer <- ask
      embed
        (modifyTVarIO
           peer
           (\p -> p {Self.fingers = update i new (Self.fingers p)}))
    OwnAddress -> Self.address <$> getPeer
    Successor -> do
        succs <- Self.successors <$> getPeer
        case succs of
          [] -> Self.address <$> getPeer
          x:_ -> return x
    Successors -> Self.successors <$> getPeer
    Predecessor -> Self.predecessor <$> getPeer
    Fingers -> return fingerCount
    ClosestPrecedingNode k -> do
        peer <- getPeer 
        let 
          fs = Self.fingers peer
          n = Self.address peer
          closestFinger = find (`isBetween` (n, k)) fs
          closestSucc = find (`isBetween` (n, k)) (reverse $ Self.successors peer)
        return $ case (closestFinger, closestSucc) of 
              (Just f, Nothing) -> f
              (Nothing, Just s) -> s
              (Nothing, Nothing) -> Self.address peer
              (Just f, Just s) -> if f `isBetween` (s, k) then f else s
    FingerTable -> Self.fingers <$> getPeer

runRouting :: (Member (Embed IO) r, Identify addr) => addr -> Sem (Routing addr ': r) a -> Sem r a
runRouting n s = do
  t <- embed $ newTVarIO $ createRoutingTable n
  runReader t (routingToReader s)

getPeer :: Members [ Reader (TVar x), Embed IO ] r => Sem r x
getPeer = ask >>= embed . readTVarIO
