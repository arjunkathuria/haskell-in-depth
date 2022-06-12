import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, withAsync)
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, stdout)

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
  hSetBuffering stdout NoBuffering
  putStrLn "Doing Something Useful"
  withAsync (printDots oneSec) $ \_ -> doSomethingUseful
  putStrLn "Exiting..."
