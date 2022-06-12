module Factorial
  ( factorial
  , factorialStrict
  ) where

factorial :: Integer -> Integer -> Integer
factorial 0 acc = acc
factorial n acc = factorial (n - 1) (n * acc)

factorialStrict :: Integer -> Integer -> Integer
factorialStrict 0 acc = acc
factorialStrict x acc = factorialStrict (x - 1) $! (x * acc)
