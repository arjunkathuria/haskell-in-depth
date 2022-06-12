import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Random (getStdRandom, uniform)

oneSec :: Int
oneSec = 1000000

doSomethingUseful :: IO ()
doSomethingUseful = do
  threadDelay $ 10 * oneSec
  putStrLn "All Done"

printDots :: Int -> IO ()
printDots msec = forever $ do
  putStrLn "."
  threadDelay msec

main :: IO ()
main = do
  putStrLn "Doing Something useful"
  withAsync (printDots oneSec) $ \_ ->
    withAsync doSomethingUseful $ \useful -> do
      threadDelay (2 * oneSec)
      interrupt <- getStdRandom uniform -- cancels based on if this is True randomly
      case interrupt of
        True -> cancel useful
        False -> wait useful >> pure ()
  putStrLn "Exiting..."
