import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

-- from now on we regard a vocabulary as consisting of entries with words and numbers of occurrences.
type Entry = (T.Text, Int)

-- and a Vocabulary is just a list of above mentioned entries.
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort $ ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    cleanWord = T.dropAround (not . isLetter)
    buildEntry xs@(x : _) = (x, length xs)

-- once we have the vocabular, we can print all the words as follows:
printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

printWordsCount :: Vocabulary -> IO ()
printWordsCount vocab = do
  let ln = length vocab
  putStrLn $ "Total number of words: " <> show ln

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab
  printWordsCount vocab

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      processTextFile fname
    _ -> putStrLn "Usage: provide a valid filename as only argument."
