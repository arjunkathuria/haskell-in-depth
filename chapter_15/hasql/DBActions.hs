module DBActions where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session

import Data.Vector (Vector)
import FilmInfo.Data (FilmInfo, Rating, printFilm)
import qualified Sessions as Ses
import Data.Int (Int64)

import Data.Text (Text)

handleError :: IO (Either Session.QueryError a) -> IO a
handleError m = do
  r <- m
  case r of
    Right v -> pure v
    Left err -> error $ show err

allFilms :: Connection -> IO (Vector FilmInfo)
allFilms conn = handleError $ Session.run Ses.allFilms conn

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = handleError $ Session.run Ses.countFilms conn

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn newRating filmName =
  handleError $ Session.run (Ses.setRating newRating filmName) conn

assignCategory :: Connection -> Text -> Text -> IO Int64
assignCategory conn catName filmName =
  handleError $ Session.run (Ses.assignCategory catName filmName) conn

printAllFilms :: Connection -> IO ()
printAllFilms conn = handleError $ Session.run (Ses.processAllFilms printFilm) conn
