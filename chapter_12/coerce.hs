{-# Language RoleAnnotations #-}

import Data.Coerce (coerce)

newtype Age = Age Int
  deriving (Show)

toAge :: [Int] -> [Age]
toAge = coerce

maybeToAge :: Maybe Int -> Maybe Age
maybeToAge = coerce

data Student ageType = Student String ageType

check :: Student Int -> Student Age
check = coerce

data Student1 ageType = Student1 String (Maybe ageType)

check1 :: Student1 Int -> Student1 Age
check1 = coerce

-- data Student2 m ageType = Student2 String (m ageType)
-- check2 :: Student2 Maybe Int -> Student2 Maybe Age
-- check2 = coerce

type role Student' nominal
data Student' ageType = Student' String ageType

main :: IO ()
main = do
  print $ toAge [1..10]
  print $ map maybeToAge $ map Just [1..10]
