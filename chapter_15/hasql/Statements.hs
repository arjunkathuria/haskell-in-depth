{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Statements where

import Data.Bifunctor (bimap, first)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust)
import Data.Profunctor (dimap, lmap, rmap)
import Data.Text (Text)
import Data.Vector (Vector)
import FilmInfo.Data
import Hasql.Statement (Statement (Statement))
import Hasql.TH
  ( maybeStatement
  , rowsAffectedStatement
  , singletonStatement
  , vectorStatement
  )

import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

countFilms :: Statement () Int64
countFilms = [singletonStatement| SELECT count (*) :: int8 FROM film |]

filmsLonger :: Statement FilmLength (Vector FilmInfo)
filmsLonger =
  dimap
    fromFilmLength
    (fmap fiFromTuple)
    [vectorStatement|
      SELECT film_id :: int4, title :: text
      , description :: text?, length :: int4, rating :: text?
      FROM film
      WHERE length > $1 :: int4 |]

filmsLongerNaive ::
     Statement Int32 (Vector (Int32, Text, Maybe Text, Int32, Maybe Text))
filmsLongerNaive =
  [vectorStatement|
      SELECT film_id :: int4, title :: text
      , description :: text?, length :: int4, rating :: text?
      FROM film
      WHERE length > $1 :: int4 |]

fromFilmLength :: FilmLength -> Int32
fromFilmLength (FilmLength len) = len

fiFromTuple :: (Int32, Text, Maybe Text, Int32, Maybe Text) -> FilmInfo
fiFromTuple (i, t, md, l, mr) =
  FilmInfo
    { filmId = FilmId i
    , title = t
    , description = md
    , filmLength = FilmLength l
    , rating = mr >>= toMaybeRating
    }

filmIdByTitle :: Statement Text (Maybe FilmId)
filmIdByTitle =
  rmap
    (fmap FilmId)
    [maybeStatement|
      SELECT film_id :: int4 FROM film
      WHERE title = $1 :: text |]

setRating :: Statement (Rating, Text) Int64
setRating =
  lmap
    (first fromRating)
    [rowsAffectedStatement|
      UPDATE film SET rating = $1 :: text :: mpaa_rating
      WHERE title = $2 :: Text |]

fromFilmId :: FilmId -> Int32
fromFilmId (FilmId i) = i

fromCatId :: CatId -> Int32
fromCatId (CatId a) = a

-- checks if a category is already assigned
isAssigned :: Statement (CatId, FilmId) Bool
isAssigned =
  dimap
    (bimap fromCatId fromFilmId)
    isJust
    [maybeStatement|
      SELECT category_id::int4 FROM film_category
      WHERE category_id=$1::int4 AND film_id=$2::int4 |]

findCatById :: Statement CatId (Maybe Text)
findCatById =
  lmap
    fromCatId
    [maybeStatement| SELECT name::text FROM category
                   WHERE category_id=$1::int4 |]

catIdByName :: Statement Text (Maybe CatId)
catIdByName =
  rmap
    (fmap CatId)
    [maybeStatement|
      SELECT category_id::int4 FROM category WHERE name=$1::text
    |]

newCategory :: Statement Text CatId
newCategory =
  rmap
    CatId
    [singletonStatement|
      INSERT INTO category (name) VALUES ($1::text)
      RETURNING category_id::int4
    |]

assignCategory :: Statement (CatId, FilmId) Int64
assignCategory =
  lmap
    (bimap fromCatId fromFilmId)
    [rowsAffectedStatement|
      INSERT INTO film_category (category_id, film_id)
      VALUES ($1::int4, $2::int4)
  |]

allFilms :: Statement () (Vector FilmInfo)
allFilms =
  rmap
    (fmap fiFromTuple)
    [vectorStatement|
      SELECT film_id :: int4, title :: text
      , description :: text?, length :: int4, rating :: text?
      FROM film
  |]

filmCategories :: Statement Text (Vector Text)
filmCategories =
  [vectorStatement|
    SELECT category.name :: text from film
    JOIN film_category USING (film_id)
    JOIN category USING (category_id)
    WHERE title = $1 :: text
    |]


fetchFilmChunks :: Statement () (Vector FilmInfo)
fetchFilmChunks =
  Statement
    "FETCH FORWARD 10 from films_cursor"
    Enc.noParams
    decoder
    True
    where
      decoder = Dec.rowVector $
        FilmInfo
        <$> (fmap FilmId . Dec.column . Dec.nonNullable) Dec.int4
        <*> (Dec.column . Dec.nonNullable) Dec.text
        <*> (Dec.column . Dec.nullable ) Dec.text
        <*> (fmap FilmLength . Dec.column . Dec.nonNullable) Dec.int4
        <*> (fmap (>>= toMaybeRating) . Dec.column . Dec.nullable ) Dec.text
