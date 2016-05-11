{-# LANGUAGE FlexibleContexts #-}

module Text.Parsing.List.Simple (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                 parse, simpleParse) where

import Prelude hiding (null, any)
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

newtype Parser lbl lst a = Parser (lst -> [(a, lst)])

instance Functor (Parser lbl lst) where
  fmap f (Parser p) = Parser (map (mapFst f) . p)

instance Applicative (Parser lbl lst) where
  pure x  = Parser (\str -> [(x, str)])
  (Parser pf) <*> (Parser pa) = Parser (concatMap combine . pf)
    where combine (f, str) = map (mapFst f) (pa str)
  (Parser pa) <* (Parser pb) = Parser (concatMap combine . pa)
    where combine (a, str1) = map (replaceFst a) (pb str1)
  (Parser pa) *> (Parser pb) = Parser (concatMap (pb . snd) . pa)

recurParse :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParse p as str = map (mapFst reverse) (go as str)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

instance Alternative (Parser lbl lst) where
  empty = Parser (const [])
  (Parser pl) <|> (Parser pr) = Parser (\str -> pl str ++ pr str)
  many (Parser p) = Parser (recurParse p [])
  some (Parser p) = Parser (concatMap f . p)
    where f (x, str) = recurParse p [x] str

instance Monad (Parser lbl lst) where
  (Parser p) >>= f = Parser (concatMap combine . p)
    where combine (a, str) = pb str
            where Parser pb = f a

instance MonadPlus (Parser lbl lst)

any :: ListLike lst elt => Parser lbl lst elt
any = Parser p
  where p str = uncons [] onCons str
        onCons x xs = [(x, xs)]

satisfy :: ListLike lst elt => (elt -> Bool) -> Parser lbl lst elt
satisfy pred = Parser p
  where p str = uncons [] onCons str
        onCons x xs = if pred x then [(x, xs)] else empty

char :: (Eq elt, ListLike lst elt) => elt -> Parser lbl lst elt
char c = satisfy (== c)

oneOf :: (ListLike lst elt, FS.HasFastSet elt) => [elt] -> Parser lbl lst elt
oneOf lst = satisfy pred
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: ListLike lst Char => Parser lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: ListLike lst Char => Parser lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string :: ListLike lst elt => lst -> Parser lbl lst lst
string str = Parser p 
  where p = dropPrefix str [] onSuccess
        onSuccess rest = [(str, rest)]

sequenceP :: (Traversable t, Eq elt, ListLike lst elt) => t elt -> Parser lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

parse :: ListLike lst elt => Parser lbl lst a -> lst -> [(a, lst)]
parse (Parser p) = p

simpleParse :: ListLike lst elt => Parser lbl lst a -> lst -> [a]
simpleParse (Parser p) = map fst . filter (null . snd) . p
