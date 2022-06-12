{-# LANGUAGE OverloadedStrings #-}

module Sessions where

import Hasql.Session (Session)
import qualified Hasql.Session as Session

import Data.Int (Int64)
import Data.Text (Text)
import FilmInfo.Data
import qualified Statements as Stmt

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)

countFilms :: Session Int64
countFilms = Session.statement () Stmt.countFilms

assignCategory :: Text -> Text -> Session Int64
assignCategory catName filmTitle = do
  catId <- findOrAddCategory catName
  mFilmId <- Session.statement filmTitle Stmt.filmIdByTitle
  case mFilmId of
    Nothing -> pure 0
    Just fid -> assignUnlessAssigned catId fid

findOrAddCategory :: Text -> Session CatId
findOrAddCategory catName = do
  cats <- Session.statement catName Stmt.catIdByName
  case cats of
    Nothing -> Session.statement catName Stmt.newCategory
    Just cid -> pure cid

assignUnlessAssigned :: CatId -> FilmId -> Session Int64
assignUnlessAssigned cid fid = do
  b <- Session.statement (cid, fid) Stmt.isAssigned
  if b
    then pure 0
    else Session.statement (cid, fid) Stmt.assignCategory

allFilms :: Session (Vector FilmInfo)
allFilms = Session.statement () Stmt.allFilms

setRating :: Rating -> Text -> Session Int64
setRating rating fname =
  Session.statement (rating, fname) Stmt.setRating

processAllFilms :: (FilmInfo -> IO ()) -> Session ()
processAllFilms process = do
  Session.sql "BEGIN"
  Session.sql declareCursor
  fetchRowsLoop
  Session.sql "END"
  where
    declareCursor =
      "DECLARE films_cursor CURSOR FOR "
      <> "SELECT film_id, title, description, "
      <> "       length, rating from film"

    fetchRowsLoop = do
      rows <- Session.statement () Stmt.fetchFilmChunks
      unless (V.null rows) $ do
        liftIO (V.mapM_ process rows)
        fetchRowsLoop
