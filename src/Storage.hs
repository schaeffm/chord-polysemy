module Storage where
import Polysemy
import Polysemy.Reader
import Control.Concurrent.STM
import qualified Data.Map as Map

import Identifier
import Util

type Value = Integer

data Storage m a where
    StorageGet :: Identifier -> Storage m (Maybe Value)
    StoragePut :: Identifier -> Value -> Storage m ()
makeSem ''Storage

storageToReader :: (Member (Embed IO) r) => Sem (Storage ': r) a -> 
    Sem (Reader (TVar (Map.Map Identifier Value)) ': r) a
storageToReader = reinterpret $ \case
    StorageGet k -> do
        storage <- ask
        Map.lookup k <$> embed(readTVarIO storage)
    StoragePut k v -> do
        storage <- ask
        embed $ modifyTVarIO storage (Map.insert k v)

runStorage :: Member (Embed IO) r => Sem (Storage ': r) a -> Sem r a
runStorage s = do
    t <- embed (newTVarIO (Map.empty))
    runReader t (storageToReader s)
