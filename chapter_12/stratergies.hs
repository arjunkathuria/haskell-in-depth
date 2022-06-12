{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}

-- for ghci use
-- :set -packge aeson

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)

newtype Age = Age {age :: Int}
  deriving stock (Show, Generic)
  deriving newtype (Num)
  deriving anyclass (ToJSON)

theAge :: Age
theAge = 30

main :: IO ()
main = do
  print theAge
  print $ encode theAge
