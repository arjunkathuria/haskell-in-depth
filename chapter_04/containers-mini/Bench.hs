import Data.List
import Data.Deque as D
import Data.Stack as S
import System.TimeIt

fill n ins s = foldl (flip ins) s [1..n]

sumAll st view remove =
  sum $ unfoldr iter st
    where
      iter s = view s >>= \x -> Just (x, remove s)

main = do
  let n = 10 ^ 6
  putStrLn "Stack: "
  timeItNamed "Stack" $
    print $ sumAll (fill n push S.empty) top pop
  timeItNamed "Deque" $
    print $ sumAll (fill n push_front D.empty) front pop_front
