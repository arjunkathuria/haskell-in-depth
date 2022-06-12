{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import GHC.Generics
import GHC.TypeLits

data Status = Ok | Err | Lol
  deriving (Generic, Show)
