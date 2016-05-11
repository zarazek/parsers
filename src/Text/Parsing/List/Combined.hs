{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Text.Parsing.List.Combined (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                   parse, simpleParse) where

import Prelude hiding (null, any)
import Text.Parsing.StaticInfo
import qualified Data.MemoCombinators as Memo
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus, mfilter, join)
import Data.ListLike
import qualified Data.FastSet as FS
import Data.Char (ord)
import Data.List (foldl')

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

data Parser elt lbl lst a = Parser (lst -> [(a, lst)]) (Maybe (StaticInfo elt))

listMemoizer :: (String -> r) -> String -> r
listMemoizer = Memo.list Memo.char

memo :: ListLike lst Char => Memo.Memo lst
memo = Memo.wrap fromList toList listMemoizer

instance ListLike lst Char => Functor (Parser Char lbl lst) where
  fmap f (Parser p s) = Parser memoP' s
    where memoP' = memo p'
          p' = map (mapFst f) . p

instance ListLike lst Char => Applicative (Parser Char lbl lst) where
  pure x = Parser p s
    where p str = [(x, str)]
          s = Just (StaticInfo True FS.full)
  (Parser pf sf) <*> ~(Parser pa sa) = Parser memoP s
    where memoP = memo p
          p = concatMap combine . pf
          combine (f, str) = map (mapFst f) (pa str)
          s = sequentialCompose sf sa
  ~(Parser pa sa) <* (Parser pb sb) = Parser memoP s
    where memoP = memo p
          p = concatMap combine . pa
          combine (a, str) = map (replaceFst a) (pb str)
          s = sequentialCompose sa sb
  (Parser pa sa) *> ~(Parser pb sb) = Parser memoP s
    where memoP = memo p
          p = concatMap (pb . snd) . pa
          s = sequentialCompose sa sb

recurParse :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParse p as lst = map (mapFst reverse) (go as lst)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

instance ListLike lst Char => Alternative (Parser Char lbl lst) where
  empty = Parser p s
    where p = const []
          s = Just (StaticInfo False FS.empty)
  (Parser pl sl) <|> (Parser pr sr) = Parser memoP s
    where memoP = memo p
          p str = pl' str ++ pr' str
          pl' = useStatic sl pl
          pr' = useStatic sr pr
          s = paralellCompose sl sr
  many (Parser p s) = Parser memoManyP manyS
    where memoManyP = memo manyP
          manyP = recurParse p' []
          p' = useStatic s p
          manyS = fmap setMatchesEmpty s
          setMatchesEmpty (StaticInfo _ st) = StaticInfo True st
  some (Parser p s) = Parser memoSomeP s
    where memoSomeP = memo someP
          someP = concatMap f . p'
          f (x, str) = recurParse p' [x] str
          p' = useStatic s p

instance ListLike lst Char => Monad (Parser Char lbl lst) where
  (Parser pa sa) >>= f = Parser memoP s
    where memoP = memo p
          p = concatMap combine . pa
          combine (a, str) = pb str
            where Parser pb _ = f a
          s = sequentialCompose sa Nothing

instance ListLike lst Char => MonadPlus (Parser Char lbl lst)

any :: (FS.HasFastFiniteSet elt, ListLike lst elt) => Parser elt lbl lst elt
any = Parser p s
  where p = uncons [] onCons
        onCons x xs = [(x, xs)]
        s = Just (StaticInfo False FS.full)

satisfyWithSet :: (FS.HasFastSet elt, ListLike lst elt) => (elt -> Bool) -> FS.FastSet elt -> Parser elt lbl lst elt
satisfyWithSet pred set = Parser p s
  where p = uncons [] onCons
        onCons x xs = case pred x of
                        True  -> [(x, xs)]
                        False -> []
        s = Just (StaticInfo (FS.isEmpty set) set)

satisfy :: (FS.HasFastFiniteSet elt, ListLike lst elt) => (elt -> Bool) -> Parser elt lbl lst elt
satisfy pred = satisfyWithSet pred (FS.filter pred FS.full)

char :: (Eq elt, FS.HasFastSet elt, ListLike lst elt) => elt -> Parser elt lbl lst elt
char c = satisfyWithSet (== c) (FS.fromList [c])

oneOf :: (FS.HasFastSet elt, ListLike lst elt) => [elt] -> Parser elt lbl lst elt
oneOf lst = satisfyWithSet pred set
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: ListLike lst Char => Parser Char lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: ListLike lst Char => Parser Char lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string :: (FS.HasFastFiniteSet elt, ListLike lst elt) => lst -> Parser elt lbl lst lst
string str = Parser p s
  where p = dropPrefix str [] onSuccess
        onSuccess rest = [(str, rest)]
        s = uncons onEmpty onCons str
        onEmpty = Just (StaticInfo True FS.full)
        onCons c _ = Just (StaticInfo False (FS.fromList [c]))

sequenceP :: (Eq elt, FS.HasFastSet elt, Traversable t, ListLike lst elt, Applicative (Parser elt lbl lst)) => t elt -> Parser elt lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

parse :: Parser elt lbl lst a -> lst -> [(a, lst)]
parse (Parser p _) = p

simpleParse :: ListLike lst elt => Parser elt lbl lst a -> lst -> [a]
simpleParse (Parser p _) = map fst . filter (null . snd) . p
