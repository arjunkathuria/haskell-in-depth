{-# Language DataKinds #-}
{-# Language GeneralizedNewtypeDeriving #-}

data TempUnits = F | C

newtype Temp (unit :: TempUnits) = Temp Double
  deriving (Num, Fractional)

nonsense :: Temp 'C
nonsense = 1.22

main :: IO ()
main = putStrLn "yo"
