{-# LANGUAGE FlexibleInstances #-}
{-# Language DeriveGeneric #-}
{-# Language TypeOperators #-}

module GenericSQL where

import Data.Text.Internal.Builder (Builder, fromString)
import GHC.Generics
import TextShow (TextShow, showb)

data Status = Ok | Err
  deriving Generic

class ToColumnsValuesLists f where
  toColumnsValues :: f a -> ([Builder], [Builder])

instance ToColumnsValuesLists U1 where
  toColumnsValues _ = ([], [])

instance (ToColumnsValuesLists a, ToColumnsValuesLists b) => ToColumnsValuesLists (a :*: b) where
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

instance (ToColumnsValuesLists a) =>  ToColumnsValuesLists (M1 i c a) where
  toColumnsValues (M1 a) = toColumnsValues a

instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) => ToColumnsValuesLists (M1 S c a) where
  toColumnsValues s@(M1 a) = (fromString (selName s) : columns, values)
    where
      (columns, values) = toColumnsValues a

instance TextShow a => ToColumnsValuesLists (K1 i a) where
  toColumnsValues (K1 a) = ([], [showb a])
