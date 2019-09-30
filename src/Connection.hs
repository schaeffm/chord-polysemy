module Connection where
import Polysemy

import Messages

type family Handle a
type family Socket a

data Connection a m p where
    Connect :: a -> Connection a m (Maybe (Handle a))
    Send :: Handle a -> P2PMsg a -> Connection a m ()
    Recv :: Handle a -> Connection a m (Maybe (P2PMsg a))
    Bind :: a -> Connection a m (Socket a)
    Listen :: Socket a -> Connection a m (Handle a)
makeSem ''Connection

data NetworkError = NoReply | NoConnection

instance Show NetworkError where
    show NoReply = "No reply from peer"
    show NoConnection = "Connection failed"

remoteCall :: Member (Connection addr) r => addr -> P2PMsg addr -> Sem r (Either NetworkError (P2PMsg addr))
remoteCall addr msg = 
    connect addr >>= \case
        Nothing -> return (Left NoConnection)
        Just con -> do
            send con msg
            recv con >>= \case
                Just reply -> return $ Right reply
                Nothing -> return $ Left NoReply

