import Control.Concurrent
import Control.Exception (onException)

oneSec :: Int
oneSec = 1000000

acquire :: IO ()
acquire = do
  putStrLn "Start Resource acquisition"
  threadDelay oneSec
  putStrLn "Resource is acquired"

release :: IO ()
release = do
  putStrLn "Start releasing the resource"
  threadDelay oneSec
  putStrLn "Resource released"

use :: IO ()
use = do
  putStrLn "Beginning using the resource"
  threadDelay (2 * oneSec)
  putStrLn "End using the Resource"

workWithResource :: IO ()
workWithResource = do
  acquire
  use `onException` release
  release

experiment :: Int -> IO () -> IO ()
experiment timeout action = do
  thr <- forkIO action
  threadDelay timeout
  killThread thr
  threadDelay (1 * oneSec)

main :: IO ()
main = do
  putStrLn "Async exception during `acquire`"
  experiment (oneSec `div` 2) workWithResource

  putStrLn "\nAsync exception during `use`"
  experiment (oneSec + oneSec `div` 2) workWithResource

  putStrLn "\nAsync exception during `release`"
  experiment (3 * oneSec + oneSec `div` 2) workWithResource
