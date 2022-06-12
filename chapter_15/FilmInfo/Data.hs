{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module FilmInfo.Data where

import Data.Int (Int32)
import Data.String (IsString)
import Data.Text (Text)
import TextShow
import Data.Text.IO as T

newtype FilmId' a = FilmId a
  deriving newtype Show
type FilmId = FilmId' Int32

newtype CatId' a = CatId a
  deriving newtype Show
type CatId = CatId' Int32

newtype FilmLength' a = FilmLength a
  deriving newtype Show
type FilmLength = FilmLength' Int32

data Rating = G | PG | PG13 | R | NC17
  deriving Show

fromRating :: IsString p => Rating -> p
fromRating G = "G"
fromRating PG = "PG"
fromRating PG13 = "PG-13"
fromRating R = "R"
fromRating NC17 = "NC-17"

toMaybeRating :: (Eq p, IsString p) => p -> Maybe Rating
toMaybeRating "G" = Just G
toMaybeRating "PG" = Just PG
toMaybeRating "PG-13" = Just PG13
toMaybeRating "R" = Just R
toMaybeRating "NC-17" = Just NC17
toMaybeRating _ = Nothing

data FilmInfo' i t d l r = FilmInfo {
    filmId :: i
  , title :: t
  , description :: d    -- nullable
  , filmLength :: l
  , rating :: r         -- nullable
  } deriving Show

-- here Maybes capture nullable fields
type FilmInfo = FilmInfo' FilmId Text (Maybe Text) FilmLength (Maybe Rating)

data FilmCategories = FilmCategories FilmInfo [Text]

data PrintDesc = WithDescription | NoDescription

instance TextShow FilmLength where
  showb (FilmLength l) = showb l <> " min"

instance TextShow FilmInfo where
  showb = filmBuilder NoDescription

filmBuilder :: PrintDesc -> FilmInfo -> Builder
filmBuilder printDesc FilmInfo {..} =
  fromText title <> " (" <> showb filmLength
  <> case rating of
       Just r -> ", " <> fromRating r <> ")"
       Nothing -> ")"
  <> case (printDesc, description) of
       (WithDescription, Just desc) -> "\n" <> fromText desc
       _ -> ""

printFilm :: FilmInfo -> IO ()
printFilm = T.putStrLn . toText . filmBuilder WithDescription

instance TextShow FilmCategories where
  showb (FilmCategories f cats) = showb f <> "\n" <> showbList cats
