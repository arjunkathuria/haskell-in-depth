import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, race, race_, wait, waitAny, waitEither, withAsync)
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

waitEnter :: IO ()
waitEnter = getLine >> pure ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Doing Something useful"

  -- The more manual / explicit way

  -- withAsync (printDots oneSec) $ \dots ->
  --   withAsync waitEnter $ \enter ->
  --     withAsync doSomethingUseful $ \useful ->
  --       waitAny [dots, enter, useful]

  -- The more abstract way
  -- we do away with all explicit thread spawning
  race_ (printDots oneSec) $ race_ waitEnter doSomethingUseful

  putStrLn "Exiting..."
