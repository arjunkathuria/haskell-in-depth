import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import Control.Monad
import Text.Read

oneSec :: Int
oneSec = 1000000

-- publisher writes to a channel
publisher :: TMChan Int -> IO ()
publisher chan = do
  putStrLn "Enter next number: "
  x' <- readMaybe <$> getLine
  case x' of
    Just x -> do
      atomically $ writeTMChan chan x    -- sends a number
      threadDelay oneSec
      publisher chan
    Nothing -> atomically $ closeTMChan chan

-- printer reads from the queue and prints
printer :: TQueue (Int, Int) -> IO ()
printer q = forever $ do
  (n, x) <- atomically $ readTQueue q
  putStrLn $ "Subscriber " <> show n
             <> " Received number " <> show x

-- The subscriber reads from a channel and writes to a destination q
subscriber :: Int -> TMChan Int -> TQueue (Int, Int) -> IO ()
subscriber n inchan outq = loop
  where
    loop = do
      next <- atomically $ readTMChan inchan -- reads a number
      case next of
        Just x -> do
          atomically $ writeTQueue outq (n, x)  -- writes to queue
          loop
        Nothing -> pure ()   -- job is done because the channel is closed

-- we'll need several subscribers
spawnSubscribers :: Int -> TMChan Int -> TQueue (Int, Int) -> IO [Async ()]
spawnSubscribers total ch outq =
  forM [1..total] $ \n -> do
    inch <- atomically $ dupTMChan ch   -- duplicates a channel (but why?)
    -- Answer
    -- we have to duplicate a channel before giving it to a subscriber
    -- otherwise, we'd have no broadcasting (this is why)
    async $ subscriber n inch outq

main :: IO ()
main = do
  outq <- newTQueueIO
  withAsync (printer outq) $ \_ -> do
    ch <- newBroadcastTMChanIO
    subs <- spawnSubscribers 10 ch outq
    publisher ch
    mapM_ wait subs

  putStrLn "Exiting..."
