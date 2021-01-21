{-# LANGUAGE StandaloneDeriving #-}

import Radar

import System.Random
import Control.Monad (replicateM, when, unless)
import Data.List (sort, nub)
import System.Exit

deriving instance Ord Turn

instance Random Direction where
  randomR (lo, hi) g = (toEnum i, g')
    where (i, g') = randomR (fromEnum lo, fromEnum hi) g

  random = randomR (minBound, maxBound)

instance Random Turn where
  randomR (lo, hi) g = (toEnum i, g')
    where (i, g') = randomR (fromEnum lo, fromEnum hi) g

  random = randomR(minBound, maxBound)

randomsIO :: Random a => Int -> IO [a]
randomsIO n = replicateM n randomIO

randomTurns :: Int -> IO [Turn]
randomTurns = randomsIO

randomDirections :: Int -> IO [Direction]
randomDirections = randomsIO

writeRandomFile :: (Random a, Show a) =>
                   Int -> (Int -> IO [a]) -> FilePath -> IO ()

writeRandomFile n gen fname = do
  xs <- gen n
  writeFile fname $ unlines $ map show xs

test_allTurnsInUse :: Bool
test_allTurnsInUse =
  sort (nub [orient d1 d2 | d1 <- every, d2 <- every ]) == every

test_roationsMonoidAgree ts =
  and [rotateMany d ts == rotateMany' d ts | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d:_) = ds == rotateManySteps d (orientMany ds)

main :: IO ()
main = do
  ts <- randomTurns 1000
  ds <- randomDirections 1000

  when (not $ and [test_allTurnsInUse,
                  test_orientRotateAgree ds,
                  test_roationsMonoidAgree ts])
    exitFailure
