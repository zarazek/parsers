{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FunctionalDependencies, FlexibleInstances #-}

module Data.MapLike ( MapLike(..) ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ListTrie.Base.Map as M (WrappedIntMap)
import qualified Data.ListTrie.Patricia.Map as ST
import qualified Data.Trie as BT

class MapLike (m :: * -> *) (k :: *) | m -> k where
  empty :: m v
  lookup :: k -> m v -> Maybe v
  insert :: k -> v -> m v-> m v

instance MapLike (ST.TrieMap M.WrappedIntMap Char) [Char] where
  empty = ST.empty
  lookup = ST.lookup
  insert = ST.insert

instance MapLike BT.Trie B.ByteString where
  empty = BT.empty
  lookup = BT.lookup
  insert = BT.insert
