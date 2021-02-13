{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Radar

import System.Environment (getArgs)
import Fmt

deriving instance Read Turn
deriving instance Read Direction

-- READING AND SHOWING
-- -------------------

-- To use fmt with user-defined types, we have to implement
-- the Buildable type class. It requires only one method, build , which transforms every
-- value into a Builder . This datatype is used to build final report from its components.
-- An implementation is straightforward:

instance Buildable Direction where
  build North =  "N"
  build East  =  "E"
  build South =  "S"
  build West  =  "W"

instance Buildable Turn where
  build TNone   =  "--"
  build TLeft   =  "<-"
  build TRight  =  "->"
  build TAround =  "||"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
  f <- readFile fname
  let turns = map read $ lines f
      finalDir = rotateMany dir turns
      dirs = rotateManySteps dir turns

  fmtLn $ "Final Direction: " +|| finalDir ||+ ""    -- uses the Show instance, notice the +|| ||+ operators
  fmt $ nameF "Intermediate directions: " (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
  f <- readFile fname
  let dirs = map read $ lines f
      turns = orientMany dirs
  fmt $ nameF "Turns along the way: " (unwordsF turns)

-- main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname]      -> orientFromFile fname
    _                  -> putStrLn $ "Usage: locator -o filename\n" ++
                                     "       locator -r filename direction"


-- Run results

-- $ cabal run radar -- -o hid-examples/data/dirs.txt
-- output:
-- > Intermediate turns: : <- || -> <- -> --

-- $ cabal run radar -- -r hid-examples/data/turns.txt North
-- output:
-- > Final Direction: North
-- > Intermediate directions: : N W S S N W N W N N N
