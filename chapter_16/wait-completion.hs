import Control.Concurrent
import System.Random

oneSec :: Int
oneSec = 1000000

randomDelay :: IO ()
randomDelay = do
  sec <- getStdRandom $ uniformR (1, 5)
  putStrLn $ "Waiting for " <> show sec <> " seconds"
  threadDelay $ sec * oneSec

main :: IO ()
main = do
  fin <- newEmptyMVar
  _ <- forkFinally randomDelay (\_ -> putMVar fin ())
  takeMVar fin -- it blocks until there's something to take in the MVar
               -- which happens after the thread above completes/err
               -- essentially waiting on it.
  pure ()

-- extra fn
delayedStart :: IO ()
delayedStart = do
  start <- newEmptyMVar
  fin   <- newEmptyMVar
  _ <- forkFinally (takeMVar start >> randomDelay)
                   (\_ -> putMVar fin ())
  threadDelay oneSec
  putMVar start () -- thread starts
  takeMVar fin     -- thread stops
  putStrLn "Exiting..."
