import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, check, readTVar)
import Control.Concurrent.STM.TVar (registerDelay)
import System.Environment (getArgs)
import Control.Monad (forever)

oneSec :: Int
oneSec = 1000000


printDots :: IO ()
printDots = forever $ do
  putStrLn "."
  tv <- registerDelay oneSec
  atomically $ readTVar tv >>= check

main :: IO ()
main = do
  [sec] <- getArgs
  withAsync printDots $ \_ -> do
    tv <- registerDelay $ oneSec * read sec
    atomically $ readTVar tv >>= check
    putStrLn "Boom! "
