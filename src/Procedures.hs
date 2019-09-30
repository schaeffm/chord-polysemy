module Procedures where
import Polysemy

import Remote
import Identifier
import Routing


data Procedures addr m a where
  FindSuccessor :: Identifier -> Procedures addr m (Maybe addr)
makeSem ''Procedures

runProcedures ::
     (Members [Remote addr, Routing addr] r, Identify addr)
  => Sem (Procedures addr ': r) a
  -> Sem r a
runProcedures =
  interpret $ \case
    FindSuccessor k -> do
      n <- ownAddress
      s <- successor
      if k `isBetweenEnd` (n, s)
        then return $ Just s
        else do
          n' <- closestPrecedingNode k
          if n /= n' then findSuccessorR n' k
          else return Nothing
