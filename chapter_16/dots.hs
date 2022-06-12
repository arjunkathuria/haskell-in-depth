import Control.Concurrent (forkIO, getNumCapabilities, killThread, threadDelay)
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
  n <- getNumCapabilities
  hSetBuffering stdout NoBuffering
  putStrLn $ "No of capabilities: " <> show n

  putStrLn "Doing Something useful"
  dotsPrinter <- forkIO (printDots oneSec)
  doSomethingUseful
  killThread dotsPrinter
  putStrLn "Exiting..."
