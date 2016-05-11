{-# LANGUAGE TypeFamilies #-}

module Data.FastSet ( HasFastSet(..), HasFastFiniteSet(..) ) where

import qualified Data.Word as W
import qualified Data.CharSet as CS
import qualified Data.IntSet as IS

import qualified Data.List as L
import qualified Data.Bits as B
import Data.Bits ((.|.), (.&.))

class HasFastSet elt where
  data FastSet elt :: *
  empty :: FastSet elt
  isEmpty :: FastSet elt -> Bool
  fromList :: [elt] -> FastSet elt
  member :: elt -> FastSet elt -> Bool
  insert :: elt -> FastSet elt -> FastSet elt
  union :: FastSet elt -> FastSet elt -> FastSet elt
  filter :: (elt -> Bool) -> FastSet elt -> FastSet elt

instance HasFastSet () where
  newtype FastSet () = UnitSet Bool
  empty = UnitSet False
  isEmpty (UnitSet x) = not x
  fromList = UnitSet . not . null
  member _ (UnitSet x)  = x
  insert _ _ = UnitSet True
  union (UnitSet l) (UnitSet r) = UnitSet (l || r)
  filter pred (UnitSet x) = UnitSet (pred () && x)

listToWord = L.foldl' (B.setBit) 0 . map fromEnum

instance HasFastSet Bool where
  newtype FastSet Bool = BoolSet W.Word
  empty = BoolSet 0
  isEmpty (BoolSet x) = x == 0
  fromList = BoolSet . listToWord
  member x (BoolSet set) = B.testBit set (fromEnum x)
  insert x (BoolSet set) = BoolSet (B.setBit set (fromEnum x))
  union (BoolSet l) (BoolSet r) = BoolSet (l .|. r)
  filter pred (BoolSet set) = BoolSet (set .&. listToWord (L.filter pred [False, True]))

instance HasFastSet Char where
  newtype FastSet Char = CharSet CS.CharSet
  empty = CharSet CS.empty
  isEmpty (CharSet set) = CS.null set
  fromList = CharSet . CS.fromList
  member x (CharSet set) = CS.member x set
  insert x (CharSet set) = CharSet (CS.insert x set)
  union (CharSet l) (CharSet r) = CharSet (CS.union l r)
  filter pred (CharSet set) = CharSet (CS.filter pred set)

instance HasFastSet Int where
  newtype FastSet Int = IntSet IS.IntSet
  empty = IntSet IS.empty
  isEmpty (IntSet set) = IS.null set
  fromList = IntSet . IS.fromList
  member x (IntSet set) = IS.member x set
  insert x (IntSet set) = IntSet (IS.insert x set)
  union (IntSet l) (IntSet r) = IntSet (IS.union l r)
  filter pred (IntSet set) = IntSet (IS.filter pred set)

class HasFastSet elt => HasFastFiniteSet elt where
  full :: FastSet elt

instance HasFastFiniteSet () where
  full = UnitSet True

instance HasFastFiniteSet Bool where
  full = BoolSet (listToWord [False, True])

instance HasFastFiniteSet Char where
  full = CharSet (CS.full)
