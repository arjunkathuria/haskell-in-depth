{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                     -- for when
import Data.Char
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import System.Environment

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort $ ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    cleanWord = T.dropAround (not . isLetter)
    buildEntry xs@(x : _) = (x, length xs)

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ (Down . snd))

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (total, unique)
  where
    total = sum $ map snd vocab
    unique = length vocab

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
  fmt $ nameF "All Words" $ unlinesF (allWords vocab)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
  fmt $
    nameF "Frequent Words" $
      blockListF' "" fmtEntry reportData
  where
    -- the blockListF' function formats list elements in the given way and presents them line by line.

    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab =
  fmt $
    "\nTotal Number of words: " +| total |+
    "\nUnique Number of words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocab

wordsCountReportOld vocab = T.unlines [part1, part2]
  where
    (total, unique) = wordsCount vocab
    part1 =
      T.append
        "Total Number of words: "
        (T.pack $ show total)

    part2 =
      T.append
        "Number of unique words: "
        (T.pack $ show unique)

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords num = do
  txt <- TIO.readFile fname
  let vocab = extractVocab txt
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab num

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile fname True (read num) -- if -a, print all words
    [fname, num] -> processTextFile fname False (read num)
    _ -> putStrLn "Enter a valid filename Bruh."


-- cmd: cabal run vocab3 -- ..\hid-examples-0.5\texts\hamlet.txt 10

--        or, with -a flag

-- cmd: cabal run vocab3 -- -a ..\hid-examples-0.5\texts\hamlet.txt 10
