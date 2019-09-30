module ChordPeer where
import Control.Monad (forM_)
import Polysemy

import Util
import Identifier
import Routing
import qualified Procedures
import Procedures (Procedures, runProcedures)
import Remote
import Connection
import Log

data ChordPeer addr m a where
  Create :: ChordPeer addr m ()
  Join :: addr -> ChordPeer addr m ()
  Stabilize :: ChordPeer addr m ()
  Notify :: addr -> ChordPeer addr m ()
  FixFingers :: ChordPeer addr m ()
  FindSuccessor :: Identifier -> ChordPeer addr m (Maybe addr)

makeSem ''ChordPeer

reinterpretChordPeer :: forall r a addr.
     (Members [Embed IO, Logger String, Routing addr] r, Identify addr)
  => Sem (ChordPeer addr:r) a
  -> Sem (Procedures addr:Remote addr:r) a
reinterpretChordPeer =
  reinterpret2 $ \case
    Create -> do
      n <- ownAddress
      setPredecessor Nothing
      setSuccessors [n]
      logInfo "Created peer"
    Join n' -> do
      logInfo "Bootstrapping..."
      n <- ownAddress
      s <- findSuccessorR n' (identifier n)
      whenJust s (setSuccessors . singleton)
      case s of 
        Nothing -> logError "Bootstrapping failed"
        Just s' -> logInfo $ "Bootstrapping found successor " ++ show s'
    Stabilize -> do
      logInfo "Begin stabilization"
      n <- ownAddress
      ss <- successors
      firstJustM (getSuccessorsR @addr) ss >>= \case
        Nothing -> return ()
        Just succs -> do
            logDebug "Retrieved successors of successor"
            setSuccessors (sortIdList n succs)
      s' <- successor
      notifyR s' n
      logInfo "Done stabilizing"
      routing' <- showRouting
      logDebug ("New routing table\n" ++ routing')
      
    Notify n' -> do
      n <- ownAddress
      predecessor >>= \case
        Just p
          | not (n' `isBetween` (p, n)) -> return ()
        _ -> setPredecessor (Just n')
    FixFingers -> do
      fs <- fingers
      forM_ [0 .. fs] fixFinger
    FindSuccessor k -> Procedures.findSuccessor k

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = return Nothing
firstJustM f (x:xs) = do
    res <- f x
    case res of
      Nothing -> firstJustM f xs
      Just r' -> return $ Just r' 

runChordPeer ::
     (Members [Embed IO, Connection addr, Logger String, Routing addr] r, Identify addr)
  => Sem (ChordPeer addr : r) a
  -> Sem r a
runChordPeer = runRemote . runProcedures . reinterpretChordPeer

fixFinger ::
     (Members [Routing a, Procedures a] r, Identify a) => Int -> Sem r ()
fixFinger i = do
  n <- ownAddress
  let fingerId = shiftIdentifierLog n i
  finger' <- Procedures.findSuccessor fingerId
  whenJust finger' (setFinger i)
