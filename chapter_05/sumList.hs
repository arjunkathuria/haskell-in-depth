import Control.Monad.State
import Data.Foldable

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' x = modify' (+x)

sumList :: [Integer] -> State Integer ()
sumList xs = traverse_ addItem xs

main :: IO ()
main = do
  let sumoflist = execState (sumList [1..100]) 0
  putStrLn $ "sum of list: " <> show sumoflist
