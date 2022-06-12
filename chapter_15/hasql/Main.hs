{-# LANGUAGE OverloadedStrings #-}

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection

import DBActions
import qualified Data.Vector as V
import TextShow (printT)
import FilmInfo.Data (printFilm)

demo :: Connection -> IO ()
demo conn = do
  -- printAllFilms conn
  allFilms conn >>= mapM_ printFilm . V.take 5
  putStr "\nTotal number of films: "
  totalFilmsNumber conn >>= printT



main :: IO ()
main = do
  (Right conn) <- Connection.acquire connectionSettings
  demo conn
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "pguser_1" "pguser" "sakila_films"
