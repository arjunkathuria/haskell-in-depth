type Name = String
type Age = Int
data Student = Student Name Age
  deriving (Eq)

main :: IO ()
main = putStrLn "hi"
