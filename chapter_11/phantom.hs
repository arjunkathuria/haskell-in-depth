{-# Language NoStarIsType #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

module Phantom where

-- import Data.Proxy (Proxy)

newtype Temp unit = Temp Double
  deriving (Num, Fractional)

data C
data F

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f-32)*5/9)

nonsense :: Temp (Temp F -> Temp c)
nonsense = 9

data Proxy t = Proxy

class UnitName u where
  unitName :: Proxy u -> String

instance UnitName C where
  unitName _ = "C"

instance UnitName F where
  unitName _ = "F"

instance UnitName u => Show (Temp u) where
  show (Temp x) = show x ++ "°" ++ unitName (Proxy :: Proxy u)
