module Remote where
import Polysemy
import Connection
import Identifier
import Messages
import Log
import Util

data Remote addr m a where
  FindSuccessorR :: addr -> Identifier -> Remote addr m (Maybe addr)
  GetSuccessorsR :: addr -> Remote addr m (Maybe [addr])
  GetPredecessorR :: addr -> Remote addr m (Maybe addr)
  NotifyR :: addr -> addr -> Remote addr m ()

makeSem ''Remote

runRemote :: (Members [Embed IO, Logger String, Connection addr] r, Identify addr) => Sem (Remote addr ': r) a -> Sem r a
runRemote =
  interpret $ \case
    FindSuccessorR remote k -> do
        logTrace "In FindSuccessorR"
        remoteCall remote (FindSuccessor k) >>= \case
            Right (FindSuccessorSuccess _ s) -> do
                -- TODO: check if k == k'
                logDebug $ "Found successor of " ++ show k ++ ": " ++ show s
                return $ Just s
            Right (FindSuccessorFailure _) -> do
                logDebug $ "Successor of " ++ show k ++ " could not be found"
                return Nothing
            Right _ -> do
                logWarn $ "Received wrong message type"
                return Nothing
            Left err -> do
                logDebug $ "FindSuccessorR:" ++ show err
                return Nothing
    GetSuccessorsR remote -> do
        logTrace $ "In GetSuccessorsR " ++ show remote
        remoteCall remote GetSuccessors >>= \case
            Right (Successors ss) -> do
                logDebug $ "Found successors: " ++ show ss
                return $ Just ss
            Right _ -> do
                logWarn $ "Received wrong message type"
                return Nothing
            Left err -> do
                logDebug $ "GetSuccessorsR:" ++ show err
                return Nothing
    GetPredecessorR remote -> do
        logTrace $ "In GetPredecessorR " ++ show remote
        remoteCall remote GetPredecessor >>= \case
            Right (PredecessorFound p) -> do
                logDebug $ "Found predecessor: " ++ show p
                return $ Just p
            Right PredecessorNotFound -> do
                logDebug $ "Predecessor could not be found"
                return Nothing
            Right _ -> do
                logWarn $ "Received wrong message type"
                return Nothing
            Left err -> do
                logDebug $ "GetPredecessorR:" ++ show err
                return Nothing
    NotifyR remote p ->
      whenJustM (connect remote) (flip send (Notify p))
