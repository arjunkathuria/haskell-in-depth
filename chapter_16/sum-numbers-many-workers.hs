{-# Language BangPatterns #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Monad

sendNumbers :: [Integer] -> TBMQueue Integer -> IO ()
sendNumbers xs q = do
  forM_ xs $ \x -> do
    atomically $ writeTBMQueue q x
  atomically $ closeTBMQueue q

sumNumbers :: TBMQueue Integer -> IO Integer
sumNumbers q = go 0
  where
    go !acc = do
      next <- atomically $ readTBMQueue q -- gives maybe Integer
      case next of
        Just n -> go (acc + n)
        Nothing -> pure acc

main :: IO ()
main = do
  q <- newTBMQueueIO 10000 -- new bounded closeable queue with 10k elements max
  summators <- replicateM 5 (async (sumNumbers q)) -- spawns worker summators
  _ <- async (sendNumbers [1..1000000] q) -- spawns the q manager
  res <- mapM wait summators
  putStrLn $ "Partial Sums are: "
  mapM_ print res
  putStrLn $ "Total Sum is " <> show (sum res)
