import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_, forever)
import Text.Read (readMaybe)

maxThread :: Int
maxThread = 5

waitUnless :: TVar Int -> Int -> STM ()
waitUnless tv n = do
  n' <- readTVar tv
  check $ n == n' -- Blocks unless n == n'

hello :: TVar Int -> Int -> IO ()
hello tv n = forever $ do
  atomically $ waitUnless tv n
  putStrLn $ "Hello from thread " <> show n
  atomically $ writeTVar tv 0


userLoop :: TVar Int -> IO ()
userLoop tv = do
  atomically $ waitUnless tv 0
  putStrLn $ "Enter Thread number (1 .." <> show maxThread <> "): "
  n' <- readMaybe <$> getLine
  case n' of
    Nothing -> do
      putStrLn "Invalid response, please try again. \n"
      userLoop tv

    Just n  -> if (1 <= n && n <= maxThread)
      then do
        atomically $ writeTVar tv n
        userLoop tv
      else do
        putStrLn "Invalid response, please try again.\n"
        userLoop tv

main :: IO ()
main = do
  tv <- atomically $ newTVar 0
  forM_ [1..maxThread] $ async . hello tv
  userLoop tv
