{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import Data.Attoparsec.ByteString.Char8 as A
  (Parser, takeWhile, char, sepBy1, parseOnly, endOfLine, endOfInput, manyTill)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Resource (runResourceT)

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as BS
import Data.Attoparsec.ByteString.Streaming

import Data.Function ((&))
import Data.Functor (void)

field :: Parser ByteString
field = A.takeWhile (\c -> c /= ',' && c /= '\r' && c /= '\n')

textField :: Parser Text
textField = T.decodeUtf8 <$> field

record :: Parser [Text]
record = textField `sepBy1` (char ',')

endOfFile :: Parser ()
endOfFile = endOfInput <|> endOfLine *> endOfInput

file :: Parser [[Text]]
file =
  (:) <$> record
      <*> manyTill (endOfLine *> record) endOfFile

-- normal approach
main :: IO ()
main = do
  content <- B.readFile "data/quotes.csv"
  print $ parseOnly file content

-- streaming approach
mainS :: IO ()
mainS = runResourceT $
    BS.readFile "./data/quotes.csv"
  & parsed file -- here it doesn't actually stream because this waits for explicit end Of File
                -- thus, the entire file sits in memory
  & void
  & S.print
