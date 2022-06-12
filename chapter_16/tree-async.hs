import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, withAsync)
import System.IO (BufferMode (..), hSetBuffering, stdout)

oneSec :: Int
oneSec = 1000000

data BinTree = Node Int BinTree BinTree | EmptyTree

-- doesn't seem to follow left is smaller and right is greater
-- but whatever

tree :: BinTree
tree =
  Node
    2
    (Node 3 EmptyTree EmptyTree)
    (Node 1 EmptyTree EmptyTree)

treeMax :: BinTree -> Int
treeMax (EmptyTree) = 0
treeMax (Node n left right) =
  max n $ max (treeMax left) (treeMax right)

-- processing a node with value 'n' takes exactly n seconds
work :: Int -> IO ()
work sec = do
  threadDelay (sec * oneSec)
  putStrLn $ "work is completed for " <> show sec <> " sec"

{- Strategy 1
we spawn processes and they run independently of each other.
-}
spawnTree :: BinTree -> IO ()
spawnTree (EmptyTree) = pure ()
spawnTree (Node n left right) = do
  _ <- async (spawnTree left)
  _ <- async (spawnTree right)
  work n

{- Strategy 2
Alternatively, we establish a parent/child dependency and
kill children whenever the parent thread completes
-}
spawnTreeCancel :: BinTree -> IO ()
spawnTreeCancel (EmptyTree) = pure ()
spawnTreeCancel (Node n left right) = do
  withAsync (spawnTree left) $ \_ ->
    withAsync (spawnTree right) $ \_ ->
      work n

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "No Children cancellation"
  spawnTree tree
  threadDelay $ (1 + treeMax tree) * oneSec

  putStrLn "\n With Children cancellation"
  spawnTreeCancel tree
  threadDelay $ (1 + treeMax tree) * oneSec

  putStrLn "Exiting..."
