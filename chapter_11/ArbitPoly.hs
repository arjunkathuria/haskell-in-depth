{-# LANGUAGE RankNTypes #-}

inc :: Num a => a -> a
inc = (+ 1)

inc' :: Int -> Int
inc' = (+ 1)

processInts :: (Int -> Int) -> [Int] -> [Int] -- rank 0 polymorphic
processInts f x = map f x

processInts' :: Num a => (a -> a) -> [Int] -> [Int] -- rank 1 polymorphic
processInts' f x = undefined -- map f x              [gives type error tho]

processInts'' ::
     (forall a. Num a =>
                  (a -> a))
  -> [Int]
  -> [Int] -- rank 2 polymorphic
processInts'' f x = map f x -- only allows functions that are exclusively Num a => a -> a only

newtype NumModifier = NumModifier {
  run :: forall a. Num a => a -> a
  }

processIntz :: NumModifier -> [Int] -> [Int]
processIntz f xs = map (run f) xs

main :: IO ()
main = putStrLn "hi"
