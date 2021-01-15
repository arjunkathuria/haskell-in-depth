import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment -- module to read command-line arguments

main = do
  args <- getArgs
  let [fname] = args

  text <- TIO.readFile fname

  let ws =
        map head $
          group $
            sort $
              map T.toCaseFold $
                filter (not . T.null) $
                  map (T.dropAround (not . isLetter)) $
                    T.words text

  -- TIO.putStrLn $ T.unwords ws
  putStr "\n"
  print $ "Number of unique words: " <> show (length ws)
  putStr "\n"
