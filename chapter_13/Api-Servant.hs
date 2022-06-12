{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 as H (Html, toHtml, b)
import Data.Aeson (ToJSON)
import Network.Wai.Handler.Warp (run)

import GHC.Generics

data Rating = Bad | Good | Great
  deriving (Show, Generic, ToJSON)

data ServiceStatus = Ok | Down
  deriving (Show, Generic, ToJSON)

type BookID = Int

type BookApi = Get '[JSON] ServiceStatus
               :<|> "title" :> Capture "id" BookID :> Get '[HTML] H.Html
               :<|> "year" :> Capture "id" BookID :> Get '[JSON] Int
               :<|> "rating" :> Capture "id" BookID :> Get '[JSON] Rating

impl :: Server BookApi
impl = pure Ok
       :<|> title
       :<|> year
       :<|> rating
  where
    title _ = pure $ H.toHtml $ H.b "Hi Arjun, you've read quite a bit of Haskell In Depth!"
    year _ = pure 2022
    rating _ = pure Great

app :: Application
app = serve (Proxy :: Proxy BookApi) impl

main :: IO ()
main = run 8081 app
