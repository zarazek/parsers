{-# LANGUAGE TypeFamilies #-}

module Data.BestMap ( BestMap(..) ) where

import qualified Data.IntMap.CharMap as CM
import qualified Data.ListTree.Patricia.Map as LT
import qualified Data.Trie as BT

type family BestMap k :: * -> *
BestMap [Char] a = LT.TrieMap (CM.CharMap a) Char a
BestMap ByteString = BT.Trie
