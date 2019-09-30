{-# LANGUAGE OverloadedStrings #-}

module MockTest where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (forM_, when, (<=<))
import Control.Applicative ((<|>))
import Data.DoubleWord
import System.Timeout
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.Async
import Polysemy.Reader
import Polysemy.State
import Text.Read (readMaybe)

import ChordPeer
import Connection
import Identifier
import Messages
import Peer
import Routing hiding (showRouting)
import RoutingTable
import Storage
import Log

mockTimeout :: Int
mockTimeout = 1*10^(6::Int)

data LogConfig m a where
    GetLogLvlTVar :: LogConfig m (TVar LogLevel)
makeSem ''LogConfig

runLogConfig :: TVar LogLevel -> Sem (LogConfig : r) a -> Sem r a
runLogConfig tv = runReader tv . (reinterpret $ \GetLogLvlTVar -> ask)

type MockId = Word256

data MockAddr = P2PAddr MockId | ApiAddr MockId
  deriving (Eq, Ord)

instance Read MockAddr where
    readsPrec i s = map (\(w,rs) -> (ApiAddr w, rs)) $ readsPrec i s 

instance Show MockAddr where
    show (ApiAddr a) = show a
    show (P2PAddr a) = show a

type MockMsgChan = TChan (P2PMsg MockAddr)

type MockConn = (MockMsgChan, MockMsgChan)

type MockSocket = TChan (MockMsgChan, TChan MockMsgChan)

type MockNetwork = TVar (Map.Map MockAddr MockSocket)

type instance Handle MockAddr = (MockMsgChan, MockMsgChan)

type instance Socket MockAddr = MockSocket

instance Identify MockAddr where
  identifier (ApiAddr x) = x
  identifier (P2PAddr x) = x

-- block (retry) until the delay TVar is set to True
fini :: TVar Bool -> STM ()
fini = check <=< readTVar

-- Read the next value from a TChan or timeout
readTChanTimeout :: Int -> TChan a -> IO (Maybe a)
readTChanTimeout timeoutAfter pktChannel = do
  readDelay <- registerDelay timeoutAfter
  atomically $
        Just <$> readTChan pktChannel
    <|> pure Nothing <* fini readDelay

reinterpretConnectionMock ::
     Member (Embed IO) r
  => Sem (Connection MockAddr:r) a
  -> Sem (Reader MockNetwork:r) a
reinterpretConnectionMock =
  reinterpret $ \case
    Connect addr -> do
      network <- ask
      embed $ do
        nw <- readTVarIO network
        case Map.lookup addr nw of
          Nothing -> do
              return Nothing
          Just peer -> do
            connCh <- atomically newTChan
            rx <- atomically newTChan
            atomically $ writeTChan peer (rx, connCh)
            timeout mockTimeout (atomically $ readTChan connCh) >>= \case
                Just tx -> do
                    return $ Just (rx, tx)
                Nothing -> do
                    return Nothing
    Send (_, tx) msg -> embed $ atomically $ writeTChan tx msg
    Recv (rx, _) -> embed $ timeout mockTimeout $ atomically $ readTChan rx
    Bind addr -> do
      network <- ask
      embed $
        atomically $ do
          nw <- readTVar network
          ch <- newTChan
          let network' = Map.insert addr ch nw
          writeTVar network network'
          return ch
    Listen sock ->
      embed $ do
        (tx, connCh) <- atomically $ readTChan sock
        rx <- atomically newTChan
        atomically $ writeTChan connCh rx
        return (rx, tx)

runConnectionMock ::
     Member (Embed IO) r
  => MockNetwork
  -> Sem (Connection MockAddr : r) a
  -> Sem r a
runConnectionMock nw c = do
  runReader nw (reinterpretConnectionMock c)

data DebugPeer =
  DebugPeer
    { storage :: TVar (Map.Map Identifier Value)
    , handle :: ThreadId
    , routing :: TVar (RoutingTable.RoutingTable MockAddr)
    }

main :: IO ()
main = do
  nw <- newTVarIO Map.empty
  logLvl <- newTVarIO I
  runM . runLogConfig logLvl . runReader nw . runConnectionMock nw . evalState (Map.empty) $ loop

loop ::
     Members [Reader MockNetwork, Connection MockAddr, Embed IO, State (Map.Map MockId DebugPeer), LogConfig] r
  => Sem r ()
loop = do
  embed $ putStrLn "Enter a command:"
  command <- embed getLine
  when (command /= "quit") $ do
    fromMaybe handleHelp $ Map.lookup command cs
    loop
  where
    cs = Map.fromList $ map (\(c, _, f) -> (c, f)) commands

commands ::
     Members [LogConfig, Reader MockNetwork, Connection MockAddr, Embed IO, State (Map.Map MockId DebugPeer)] r
  => [(String, String, Sem r ())]
commands =
  [ ("put", "Store a key-value pair in the DHT", handlePut)
  , ("get", "Retrieve value for some key from DHT", handleGet)
  , ("help", "Display this message", handleHelp)
  , ("inspect", "Inspect the network", handleInspect)
  , ("join", "Add a new node to the network", handleJoin)
  , ("create", "Open a new network", handleCreate)
  , ("kill", "Remove a node from the network", handleKill)
  , ("showlog", "Show the log", handleShowLog)
  , ("log", "Activate logging", handleLog)
  ]

readLine :: Read a => String -> IO (Maybe a)
readLine msg = do
  putStrLn msg
  readMaybe <$> getLine

handleShowLog :: Sem r ()
handleShowLog = undefined

handleLog :: Sem r ()
handleLog = undefined

handlePut :: Members [Connection MockAddr, Embed IO] r => Sem r ()
handlePut =
  embed (readLine "Enter a key:") >>= \case
    Nothing -> embed $ putStrLn "Please enter a valid key!"
    Just key ->
      embed (readLine "Enter a value") >>= \case
        Nothing -> embed $ putStrLn "Please enter a valid value!"
        Just value ->
          embed (readLine "On which peer?") >>= \case
            Nothing -> embed $ putStrLn "Invalid Address"
            Just (addr :: Word256) -> do
              let dhtPut = DhtPut key value
              connect (ApiAddr addr) >>= \case
                Nothing -> embed $ putStrLn "Connection failed!"
                Just con -> send con dhtPut

handleGet :: Members [Connection MockAddr, Embed IO] r => Sem r ()
handleGet =
  embed (readLine "Enter a key:") >>= \case
    Nothing -> embed $ putStrLn "Please enter a valid key!"
    Just key ->
      embed (readLine "On which peer?") >>= \case
        Nothing -> embed $ putStrLn "Invalid Address"
        Just addr -> do
          let dhtGet = DhtGet key
          connect (ApiAddr addr) >>= \case
            Nothing -> embed $ putStrLn "Connection failed"
            Just con -> do
              send con dhtGet
              recv con >>= \case
                Just (DhtSuccess _ value) ->
                  embed $
                  putStrLn $
                  "Received value for key " <> show key <> ": " <> show value
                _ ->
                  embed $
                  putStrLn $ "Failed to receive value for key " <> show key

showRouting :: RoutingTable MockAddr -> String
showRouting r =
  "Peer " ++
  show (RoutingTable.address r) ++
  "\n" ++
  "Successor: " ++
  show (RoutingTable.successors r) ++
  "\n" ++
  "Predecessor: " ++
  show (RoutingTable.predecessor r) ++
  "\n" ++ "Fingers: " ++ show (RoutingTable.fingers r)

showStorage :: Map.Map Identifier Value -> String
showStorage s = "Storage:\n" ++ show s

handleInspect ::
     Members [State (Map.Map MockId DebugPeer), Embed IO] r
  => Sem r ()
handleInspect = do
  peers <- get @(Map.Map MockId DebugPeer)
  forM_ peers $ \DebugPeer {routing = r, storage = s} ->
    embed $ do
      r' <- readTVarIO r
      putStrLn $ showRouting r'
      s' <- readTVarIO s
      putStrLn $ showStorage s'

handleJoin ::
     Members [LogConfig, Reader MockNetwork, Connection MockAddr, State (Map.Map MockId DebugPeer), Embed IO] r
  => Sem r ()
handleJoin =
  embed (readLine "Address of the new peer?") >>= \case
    Nothing -> embed $ putStrLn "Invalid Address"
    Just addr -> do
      embed (readLine "Bootstrap address?") >>= \case
        Nothing -> embed $ putStrLn "Invalid Address"
        Just bs -> do
          r <- embed $ newTVarIO $ createRoutingTable (P2PAddr addr)
          s <- embed $ newTVarIO $ Map.empty
          nw <- ask
          logLvl <- getLogLvlTVar
          h <- embed $ startMockPeer addr logLvl nw r s (Just (P2PAddr bs))
          ps <- get
          let peer = DebugPeer {routing = r, storage = s, handle = h}
          put (Map.insert addr peer ps)

runLoggerMock ::
    Member (Embed IO) r => MockAddr -> TVar LogLevel -> Sem (Logger String: r) a -> Sem r a
runLoggerMock addr lvlTVar = runLoggerTVarIO lvlTVar .
    (reinterpret $ \(Log lvl msg) -> Log.log lvl ("Peer " ++ show addr ++ ": " ++ msg))

runStorageMock ::
     Member (Embed IO) r
  => TVar (Map.Map Identifier Value)
  -> Sem (Storage : r) a
  -> Sem r a
runStorageMock s = runReader s . storageToReader

runRoutingMock ::
     Member (Embed IO) r
  => TVar (RoutingTable MockAddr)
  -> Sem (Routing MockAddr ': r) a
  -> Sem r a
runRoutingMock r = runReader r . routingToReader

runPeerDebug ::
     Members [Embed IO, Async, Connection MockAddr, Logger String, Delay] r
  => TVar (RoutingTable MockAddr)
  -> TVar (Map.Map Identifier Value)
  -> MockAddr
  -> Sem (Peer MockAddr:r) a
  -> Sem r a
runPeerDebug r s api =
  runRoutingMock r . runChordPeer . runStorageMock s . reinterpretPeer api

handleKill ::
     Members [Embed IO, State (Map.Map MockId DebugPeer)] r => Sem r ()
handleKill =
  embed (readLine "Address of the peer to kill?") >>= \case
    Nothing -> embed $ putStrLn "Invalid Address"
    Just addr -> do
      ps <- get
      case Map.lookup addr ps of
        Nothing -> embed $ putStrLn "Peer not in network!"
        Just (DebugPeer {handle = h}) -> do
          embed $ killThread h
          put (Map.delete addr ps)

handleHelp ::
     forall r.
     Members [LogConfig, Reader MockNetwork, Connection MockAddr, Embed IO, State (Map.Map MockId DebugPeer)] r
  => Sem r ()
handleHelp =
  forM_ (commands @r) $ \(cmd, desc, _) ->
    embed (putStrLn $ cmd ++ ": " ++ desc)

startMockPeer :: Word256 -> TVar LogLevel -> MockNetwork -> TVar (RoutingTable MockAddr) -> TVar (Map.Map Identifier Value) -> Maybe MockAddr -> IO ThreadId
startMockPeer addr logLvl nw r s bs =
        forkIO $
        runM $ asyncToIO $ runDelayIO $ runLoggerMock (P2PAddr addr) logLvl $ runConnectionMock nw $ runPeerDebug r s (ApiAddr addr) $ start bs

handleCreate ::
     Members [LogConfig, Reader MockNetwork, Connection MockAddr, State (Map.Map MockId DebugPeer), Embed IO] r
  => Sem r ()
handleCreate =
  embed (readLine "Address of the new peer?") >>= \case
    Nothing -> embed $ putStrLn "Invalid Address"
    Just addr -> do
      r <- embed $ newTVarIO $ createRoutingTable (P2PAddr addr)
      s <- embed $ newTVarIO $ Map.empty
      nw <- ask
      logLvl <- getLogLvlTVar
      h <- embed $ startMockPeer addr logLvl nw r s Nothing
      ps <- get
      let peer = DebugPeer {routing = r, storage = s, handle = h}
      embed $ putStrLn "new peer"
      put (Map.insert addr peer ps)
