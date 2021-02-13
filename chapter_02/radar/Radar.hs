{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Radar where

import Fmt
import System.Environment

--   The Problem.
--   -------------

-- A radar antenna is a device with the ability to be oriented toward four points of
-- direction (namely, north, east, south, or west).

-- We can rotate a radar antenna. It supports a limited set of commands to be turned left, right, and around,
-- Besides, it can also stay in its current direction.

-- The task is to write a program which manipulates a radar antenna. The following are
-- the two modes of operation:

--   1. when given a file with a list of turns and a starting direction,
--   the program executes all the turns and reports the final direction,
--   with all the intermediate directions;alskdjflasd

--   2. when given a file with a list of directions, the program computes
--   and reports the corresponding set of turns to orient the radar antenna as required.

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, CyclicEnum, Show)

-- Basic operations over a radar includes the following:
-- 1. rotate: -- determine a new antenna direction after rotating
-- 2. orient: -- find a rotation to change an orientation from the first given direction to the second one.

-- rotate :: Turn -> Direction -> Direction
-- orient :: Direction -> Direction -> Turn

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

-- we could have `instance CyclicEnum Direction`, but we derived it instead
-- i had to enable the language extension `DeriveAnyClass` for this.
-- since there is no minimum definition for CyclicEnum, the automagically derived instance
-- gives us the same instance as using the instance declration, in this case.

-- IMPLEMENTING RADAR MANIPULATION FUNCTIONS
-- ------------------------------------------

-- With all these instances, the definition of rotate becomes rather trivial:
rotate :: Turn -> Direction -> Direction

rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

-- scanl is like foldl, but it gives a list of successive reduced values from the left.
-- scanl f z [x1, x2, ...] = [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- also note that:
-- last (scanl f z xs) = foldl f z xs

orientMany :: [Direction] -> [Turn]
orientMany xs@(_:_:_) = zipWith orient xs (tail xs)           -- requires min 3 elements in the list?
orientMany _ = []

instance Semigroup Turn where
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty = TNone

-- rotateMany using Monoid and Semigroup

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' d ts = rotate (mconcat ts) d

-- you'd have to reduce it step by step though if you need intermediate rotation directions.
