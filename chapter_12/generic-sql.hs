{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

import Data.Text
import GHC.Generics
import GenericSQL
import Data.Text.Internal.Builder (Builder, fromText)
import qualified Data.Text.Internal.Builder as DTIB
import TextShow (showbCommaSpace, toText)

class ToSQL a where
  insertInto :: Text -> a -> Text
  default insertInto :: (Generic a, ToColumnsValuesLists (Rep a)) => Text -> a -> Text
  insertInto = insertIntoDefault

data Student =
  Student
    { studentId :: Int
    , name :: Text
    , year :: Int
    } deriving stock Generic
      deriving anyclass ToSQL

data Course =
  Course
    { courseId :: Int
    , title :: Text
    , instructor :: Text
    } deriving stock Generic
      deriving anyclass ToSQL

insertIntoDefault :: (Generic a, ToColumnsValuesLists (Rep a)) => Text -> a -> Text
insertIntoDefault table val =
  toText $ "INSERT INTO " <> fromText table <> " " <> buildersToList columns <> " VALUES " <> buildersToList values
  where
    (columns, values) = toColumnsValues (from val)

buildersToList :: [Builder] -> Builder
buildersToList [] = "()"
buildersToList (x:xs) = DTIB.singleton '(' <> x <> go xs
  where
    go (y:ys) = showbCommaSpace <> y <> go ys
    go [] = DTIB.singleton ')'


main :: IO ()
main = do
  print $ insertInto "student" $ Student 69420 "Robert Plant" 2
  print $ insertInto "course" $ Course 42069 "Intro To Physics" "Richard Feynman"
