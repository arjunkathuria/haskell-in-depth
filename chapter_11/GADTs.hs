{-# Language GADTs #-}
-- data Dyn = S String | C Char | B Bool

data DynValue a where
  S :: String -> DynValue String
  C :: Char -> DynValue Char
  B :: Bool -> DynValue Bool


getValue :: DynValue a -> a

getValue (B b) = b
getValue (C c) = c
getValue (S s) = s


data WrappedValue where
  Wrap :: DynValue a -> WrappedValue

data WrappedValue' = forall a . Wrap' (DynValue a)

data JsonNull
data JsonValue a where
  JsonString :: String -> JsonValue String
  JsonArray :: a -> JsonValue [a]
  JsonInt :: Int -> JsonValue Int
  JsonNull :: JsonValue JsonNull


toJson :: JsonValue a -> a
toJson (JsonString s) = s
