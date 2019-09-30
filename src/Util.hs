module Util where
import Control.Concurrent.STM
import Control.Monad

singleton :: a -> [a]
singleton x = [x]

modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO v f = atomically $ modifyTVar v f

update :: Int -> a -> [a] -> [a]
update i n xs = take i xs ++ n : drop (i + 1) xs

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m () 
whenJust (Just x) f = void $ f x
whenJust _ _ = return ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whenJustM m f = m >>= (`whenJust` f)
