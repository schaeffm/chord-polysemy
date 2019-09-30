module Peer where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import Polysemy
import Polysemy.Async
import Data.Maybe (maybeToList)

import ChordPeer
import Connection
import Identifier
import Log
import Messages as Msg
import Routing
import Storage
import Util

stabilizeInterval :: Int
stabilizeInterval = 3 * 10 ^ (6 :: Int)

data Peer addr m a where
  Start :: Maybe addr -> Peer addr m ()

makeSem ''Peer

data Delay m a where
  Delay :: Int -> Delay m ()

makeSem ''Delay

reinterpretPeer ::
     ( Members '[ Embed IO, Delay, Connection addr, Logger String, Async] r
     , Identify addr
     )
  => addr
  -> Sem (Peer addr : r) a
  -> Sem (Storage : ChordPeer addr : Routing addr : r) a
reinterpretPeer api =
  reinterpret3 $ \case
    Start bootstrap -> do
      create
      whenJust bootstrap join
      n <- ownAddress
      p <- async (p2pHandler n)
      logInfo "Started P2P-Handler"
      s <- async stabilizeLoop
      logInfo "Started stabilize-loop"
      a <- async (apiHandler api)
      embed $ putStrLn "started api"
      forM_ [p, s, a] await

runPeer ::
     ( Members '[ Embed IO, Delay, Connection addr, Logger String, Async] r
     , Identify addr
     )
  => addr
  -> addr
  -> Sem (Peer addr ': r) a
  -> Sem r a
runPeer n api = runRouting n . runChordPeer . runStorage . reinterpretPeer api

newPeer ::
     forall addr r. Member (Peer addr) r
  => Sem r ()
newPeer = start Nothing

runDelayIO :: Member (Embed IO) r => Sem (Delay : r) a -> Sem r a
runDelayIO = interpret $ \(Delay i) -> embed $ threadDelay i

runDelayPure :: Sem (Delay : r) a -> Sem r a
runDelayPure = interpret $ \(Delay _) -> return ()

stabilizeLoop ::
     (Members '[ Embed IO, Logger String, ChordPeer addr, Async, Delay] r)
  => Sem r ()
stabilizeLoop =
  forever $ do
    logTrace "In stabilizeLoop" 
    s <- async stabilize
    f <- async fixFingers
    forM_ [s,f] await
    logTrace "Stabilization done"
    delay stabilizeInterval

p2pHandler ::
     ( Members [Logger String, Async, ChordPeer addr, Storage, Connection addr, Embed IO, Routing addr] r
     , Identify addr
     )
  => addr
  -> Sem r ()
p2pHandler n = server n handleConnection

apiHandler ::
     ( Members [Logger String, Async, Connection addr, ChordPeer addr, Storage, Embed IO, Routing addr] r
     , Identify addr
     )
  => addr
  -> Sem r ()
apiHandler n = server n handleApiConnection

handleApiConnection :: Members [Logger String, Connection addr, ChordPeer addr] r => Handle addr -> Sem r ()
handleApiConnection con = do
    msg <- recv con
    whenJust msg $ \case
        Msg.DhtPut k v ->
            whenJustM (findSuccessor k) (\s ->
                whenJustM (connect s) (flip send (Msg.StoragePut k v)))
        Msg.DhtGet k ->
            findSuccessor k >>= \case
                Just s -> 
                    remoteCall s (Msg.StorageGet k) >>= \case
                        Right (StorageGetSuccess _ v) -> send con (DhtSuccess k v)
                        _ -> send con (DhtFailure k)
                Nothing -> send con (DhtFailure k)
        _ -> return ()

handleConnection ::
     ( Members [Logger String, Routing addr, Connection addr, ChordPeer addr, Storage] r
     , Identify addr
     )
  => Handle addr
  -> Sem r ()
handleConnection con = do
  msg <- recv con
  whenJust msg $ \case
    Msg.Notify p -> do
      logDebug $ "Handling Notify " ++ show p
      notify p
    Msg.FindSuccessor k -> do
      logDebug $ "Handling FindSuccessor " ++ show k
      findSuccessor k >>= \case
        Just s -> send con (FindSuccessorSuccess k s)
        Nothing -> send con (FindSuccessorFailure k)
    Msg.StorageGet k -> do
      logDebug $ "Handling StorageGet " ++ show k
      storageGet k >>= \case
        Just v -> send con (StorageGetSuccess k v)
        Nothing -> send con (StorageFailure k)
    Msg.StoragePut k v -> do
      logDebug $ "Handling StoragePut " ++ show k
      storagePut k v
    Msg.GetSuccessors -> do
        p <- predecessor
        ss <- successors
        n <- ownAddress
        let succs = maybeToList p ++ n:ss
        send con (Successors succs)
    Msg.GetPredecessor -> do
      logDebug $ "Handling GetPredecessor"
      predecessor >>= \case
        Just p -> send con (PredecessorFound p)
        Nothing -> send con PredecessorNotFound
    _ -> do
      logWarn $ "Cannot handle incoming message"
      return ()

server ::
     Members '[ Embed IO, Connection addr, Async] r
  => addr
  -> (Handle addr -> Sem r ())
  -> Sem r ()
server n f = do
  sock <- bind n
  forever $ listen sock >>= (async . f) >> return ()
