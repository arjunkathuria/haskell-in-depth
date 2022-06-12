{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Control.Exception (bracket)
import FilmInfo.FromField ()
import FilmInfo.Data

import GHC.Generics
import Data.Int
import Data.Text (Text)
import Data.Maybe

deriving instance Generic FilmId
deriving newtype instance FromField FilmId
deriving newtype instance ToField FilmId

deriving instance Generic CatId
deriving newtype instance FromField CatId
deriving newtype instance ToField CatId

deriving instance Generic FilmLength
deriving newtype instance FromField FilmLength
deriving newtype instance ToField FilmLength

deriving instance Generic FilmInfo
deriving instance FromRow FilmInfo

allFilms :: Connection -> IO [FilmInfo]
allFilms conn = query_ conn select
  where select = "SELECT film_id, title, description, length, rating FROM film"

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  [Only cnt] <- query_ conn "SELECT count(*) FROM film"
  pure cnt

filmsLonger :: Connection -> FilmLength -> IO [FilmInfo]
filmsLonger conn (FilmLength l) = query conn select (Only l)
  where
    select = "SELECT film_id, title, description, length, rating FROM film"
      <>  " WHERE length > ?"

catIdByName :: Connection -> Text -> IO (Maybe CatId)
catIdByName conn catName = listToMaybe . map fromOnly <$> query conn select (Only catName)
  where
    select = "SELECT category_id FROM category"
      <> " WHERE name = ?"

isAssigned :: Connection -> CatId -> FilmId -> IO Bool
isAssigned conn catId filmId = do
  [Only res] <- query conn select (catId, filmId)
  pure $ res > (0 :: Int64)
    where
      select = "SELECT count(category_id) FROM film_category"
        <> " WHERE category_id = ? AND film_id = ?"

assignUnlessAssigned :: Connection -> CatId -> FilmId -> IO Int64
assignUnlessAssigned conn cid fid = do
  b <- isAssigned conn cid fid
  case b of
    True -> pure 0
    False -> execute conn insert (cid, fid)
      where
        insert = "INSERT INTO film_category (category_id, film_id) VALUES (?, ?)"

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn fRating filmTitle =
  execute conn update (fromRating fRating :: Text, filmTitle)
  where
    update = "update film SET rating = ? where title = ?"

-- main = do
--   conn <- connectPostgreSQL connString
--   close conn
--     where
--       connString = "host=localhost dbname=sakila_films user=pguser_1 password=pguser"

demo :: Connection -> IO ()
demo _ = pure ()

main :: IO ()
main = do
  catid <- bracket (connectPostgreSQL connString) close (flip catIdByName "Documentary")
  print catid
  total <- bracket (connectPostgreSQL connString) close (totalFilmsNumber)
  print total
  pure ()
    where
      connString = "host=localhost dbname=sakila_films user=pguser_1 password=pguser"
