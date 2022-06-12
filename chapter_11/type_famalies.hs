{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Char (showLitChar)
import Unsafe.Coerce

type family Simplify t
type instance Simplify Integer = Integer

type instance Simplify Int = Integer
type instance Simplify Double = Integer
type instance Simplify String = String
type instance Simplify Char = String
type instance Simplify Bool = String

-- It is not enough to describe this transformation at the level of types. We have to
-- process our data also. As usual, with functions that should be able to process data of
-- different types, we have to define a type class

class Simplifier t where
  simplify :: t -> Simplify t

instance Simplifier Integer where
  simplify = id

instance Simplifier Double where
  simplify = round

instance Simplifier Int where
  simplify = fromIntegral

instance Simplifier String where
  simplify = id

instance Simplifier Char where
  simplify = (:"")

instance Simplifier Bool where
  simplify = show


-- closed type synonym family

type family Widen a where
  Widen Bool = Int
  Widen Int = Integer
  Widen Char = String
  Widen t = String     -- we can do this catchall thing in closed type synonym families, but NOT in open ones

class Widener a where
  widen :: a -> Widen a

instance Widener Bool where
  widen False = 0
  widen True = 1

instance Widener Int where
  widen a = fromIntegral a

instance Widener Char where
  widen c = [c]

instance Widener Double where -- the catchall case
  widen = show

-- Closed type synonym families give more information to the compiler,
-- so it is more flexible to work with them

name = "अर्जुन"

newtype UnescapingChar = UnescapingChar { unescapingChar :: Char }

instance Show UnescapingChar where
  showsPrec _ (UnescapingChar '\'') = showString "'\\''"
  showsPrec _ (UnescapingChar c) = showChar '\'' . showLitChar' c . showChar '\''

  showList cs = showChar '"' . showLitString' (map unescapingChar cs) . showChar '"'


showLitChar' :: Char -> ShowS
showLitChar' c s
  | c > '\DEL' = showChar c s
  | otherwise = showLitChar c s

showLitString' :: String -> ShowS
showLitString' [] s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c : cs) s = showLitChar' c (showLitString' cs s)

type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b :: k)  = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

class ToUnescaping a where
  toUnescaping :: a -> ToUnescapingTF a

instance Show a => ToUnescaping a where
  toUnescaping = unsafeCoerce

type UnescapingShow t = (ToUnescaping t, Show (ToUnescapingTF t))

ushow :: UnescapingShow t => t -> String
ushow = show . toUnescaping

uprint :: UnescapingShow t => t -> IO ()
uprint = putStrLn . ushow

main :: IO ()
main = uprint name
