module Ch10
  ( factorial
  , factorialStrict
  , sumRangeR
  , sumRangeL
  , sumRangeL'
  ) where

import Data.List (foldl', foldr)
import Factorial (factorial, factorialStrict)

sumRangeR :: [Integer] -> Integer
sumRangeR = foldr (+) 0

sumRangeL :: [Integer] -> Integer
sumRangeL = foldl (+) 0

sumRangeL' :: [Integer] -> Integer
sumRangeL' = foldl' (+) 0
