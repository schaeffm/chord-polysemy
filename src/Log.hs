module Log where
import Polysemy
import Polysemy.Output
import Prelude hiding (log)
import Control.Monad (when)
import Control.Concurrent.STM

data LogLevel =
    T | D | I | W | E | NoLog
    deriving (Eq, Ord)

instance Show LogLevel where
    show T = "Trace"
    show D = "Debug"
    show I = "Info"
    show W = "Warning"
    show E = "Error"
    show NoLog = "No logging"

data Logger msg m a where
    Log :: LogLevel -> msg -> Logger msg m ()
makeSem ''Logger

logTrace :: Member (Logger msg) r => msg -> Sem r ()
logTrace msg = log T msg
logDebug :: Member (Logger msg) r => msg -> Sem r ()
logDebug msg = log D msg
logInfo :: Member (Logger msg) r => msg -> Sem r ()
logInfo msg = log I msg
logWarn :: Member (Logger msg) r => msg -> Sem r ()
logWarn msg = log W msg
logError :: Member (Logger msg) r => msg -> Sem r ()
logError msg = log E msg

mkBold :: String -> String
mkBold str = "\x1b[1m" ++ str ++ "\x1b[21m"

newtype TermColor = TermColor Int

logColor :: LogLevel -> TermColor
logColor T = TermColor 37 -- light gray
logColor D = TermColor 37 -- light gray
logColor I = TermColor 34 -- blue
logColor W = TermColor 33 -- yellow
logColor E = TermColor 31 -- red
logColor NoLog = TermColor 39 -- default

mkColor :: TermColor -> String -> String
mkColor (TermColor c) str = "\x1b[" ++ show c ++ "m" ++ str ++ "\x1b[39m" 

mkBanner :: LogLevel -> String
mkBanner lvl = mkBold $ mkColor (logColor lvl) $ "[" ++ show lvl ++ "]"

runLoggerAsOutput :: Member (Output msg) r => Sem (Logger msg : r) a -> Sem r a 
runLoggerAsOutput = interpret $ \(Log _ msg) -> output msg

runLoggerAsOutputLevel :: Member (Output msg) r => LogLevel -> Sem (Logger msg : r) a -> Sem r a 
runLoggerAsOutputLevel lvlMin = interpret $ \(Log lvl msg) -> when (lvl >= lvlMin) $ output msg

runLoggerTVar :: (Monoid msg, Member (Embed IO) r) => TVar LogLevel -> TVar msg -> Sem (Logger msg : r) a -> Sem r a
runLoggerTVar lvlMinTVar v = 
    interpret $ \(Log lvl msg) -> do
        lvlMin <- embed $ readTVarIO lvlMinTVar
        when (lvl >= lvlMin) $ embed $ atomically $ modifyTVar v (<> msg)

runLoggerBasic :: Member (Embed IO) r => Sem (Logger String:r) a -> Sem r a
runLoggerBasic = interpret $ \(Log _ msg) -> embed $ putStrLn msg

runLoggerTVarIO :: Member (Embed IO) r => TVar LogLevel -> Sem (Logger String : r) a -> Sem r a
runLoggerTVarIO lvlMinTVar = 
    interpret $ \(Log lvl msg) -> do
        lvlMin <- embed $ readTVarIO lvlMinTVar
        when (lvl >= lvlMin) $ embed $ putStrLn $ mkBanner lvl ++ " " ++ msg
