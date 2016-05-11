module Data.SmallEnum where

class SmallEnum a where
  fromInt :: Int -> a
  toInt :: a -> Int
