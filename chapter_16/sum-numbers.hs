{-# Language BangPatterns  #-}

import Control.Concurrent
import Control.Monad (forM_)
import Control.Concurrent.Async

oneSec :: Int
oneSec = 1000000

sendNumbers :: [Int] -> Chan (Maybe Int) -> IO ()
sendNumbers xs ch = do
  forM_ xs $ \x -> do
    writeChan ch (Just x)
    threadDelay $ oneSec `div` 2

  writeChan ch Nothing -- nothing indicates that numbers are over

sumNumbers :: Chan (Maybe Int) -> IO Int
sumNumbers ch = go 0
  where
    go !acc = do
      num <- readChan ch
      case num of
        (Just x) -> do
                 putStrLn $ "we've got the number: " <> show x
                 go (acc + x)
        Nothing  -> do
                 putStrLn "No numbers left to read"
                 pure acc

main :: IO ()
main = do
  chan <- newChan
  summator <- async (sumNumbers chan)
  _ <- async (sendNumbers [1..5] chan)
  res <- wait summator
  putStrLn $ "sum is: " <> show res
