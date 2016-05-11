{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleInstances #-}

module Data.ListLike ( ListLike(..) ) where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

class ListLike lst elt | lst -> elt where
  null :: lst -> Bool
  cons :: elt -> lst -> lst
  uncons :: forall r. r -> (elt -> lst -> r) -> lst -> r
  dropPrefix :: forall r. lst -> r -> (lst -> r) -> lst -> r
  toList :: lst -> [elt]
  fromList :: [elt] -> lst

instance Eq a => ListLike [a] a where
  null = L.null
  cons = (:)
  uncons onEmpty onCons lst = case lst of
                                []   -> onEmpty
                                x:xs -> onCons x xs
  dropPrefix prefix onFail onSuccess = go prefix
    where go prefix lst = case prefix of
                            []   -> onSuccess lst
                            x:xs -> case lst of
                                      []               -> onFail
                                      y:ys | x == y    -> go xs ys
                                           | otherwise -> onFail
  toList = id
  fromList = id

instance ListLike B.ByteString Char where
  null = B.null
  cons = B.cons
  uncons onEmpty onCons bs = case B.uncons bs of
                               Nothing       -> onEmpty
                               Just (hd, tl) -> onCons hd tl
  dropPrefix prefix onFail onSuccess bs = case pref == prefix of
                                            False -> onFail
                                            True  -> onSuccess suf
                                            where (pref, suf) = B.splitAt (B.length prefix) bs
  toList = B.unpack
  fromList = B.pack

instance ListLike T.Text Char where
  null = T.null
  cons = T.cons
  uncons onEmpty onCons txt = case T.uncons txt of
                                Nothing       -> onEmpty
                                Just (hd, tl) -> onCons hd tl
  dropPrefix prefix onFail onSuccess bs = case pref == prefix of
                                            False -> onFail
                                            True  -> onSuccess suf
                                            where (pref, suf) = T.splitAt (T.length prefix) bs
  toList = T.unpack
  fromList = T.pack
