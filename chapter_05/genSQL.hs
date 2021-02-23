{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.Writer

-- We can apply the Writer monad as follows here:
  -- we convert every correct line to SQL
  -- we append an error message to a log for every incorrect line

type SQL = Text

data ErrorMessage = WrongFormat Int Text
  deriving Show

-- NOTE :-
-- NEVER construct SQL queries by concatenation as we did in genInsert as it is highly insecure.

genInsert :: Text -> Text -> Text
genInsert s1 s2 = "Insert INTO items VALUES ('" <> s1 <>"', '" <> s2 <> "');\n"

processLine :: (Int, Text) -> Writer [ErrorMessage] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genSQL :: Text -> Writer [ErrorMessage] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1..] $ T.lines txt)

testData :: Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors

main :: IO ()
main = testGenSQL
