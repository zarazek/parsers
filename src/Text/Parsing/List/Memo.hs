{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Text.Parsing.List.Memo (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                 parse, simpleParse) where

import Prelude hiding (null, any)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus, mfilter, join)
import Data.ListLike
import qualified Data.FastSet as FS
import Data.Char (ord)
import Data.List (foldl')
import qualified Data.MemoCombinators as Memo

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

newtype Parser lbl lst a = Parser (lst -> [(a, lst)])

listMemoizer :: (String -> r) -> String -> r
listMemoizer = Memo.list Memo.char

memo :: ListLike lst Char => Memo.Memo lst
memo = Memo.wrap fromList toList listMemoizer

instance ListLike lst Char => Functor (Parser lbl lst) where
  fmap f (Parser p) = Parser p''
    where p'' = memo p'
          p' = map (mapFst f) . p

instance ListLike lst Char => Applicative (Parser lbl lst) where
  pure x  = Parser (\str -> [(x, str)])
  (Parser pf) <*> ~(Parser pa) = Parser p
    where p = memo p'
          p' = concatMap combine . pf
          combine (f, str) = map (mapFst f) (pa str)
  ~(Parser pa) <* (Parser pb) = Parser p
    where p = memo p'
          p' = concatMap combine . pa
          combine (a, str) = map (replaceFst a) (pb str)
  (Parser pa) *> ~(Parser pb) = Parser p
    where p = memo p'
          p' = concatMap (pb . snd) . pa

recurParse :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParse p as lst = map (mapFst reverse) (go as lst)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

instance ListLike lst Char => Alternative (Parser lbl lst) where
  empty = Parser (const [])
  (Parser pl) <|> (Parser pr) = Parser p
    where p = memo p'
          p' str = pl str ++ pr str
  many (Parser p) = Parser p''
    where p'' = memo p'
          p' = recurParse p []
  some (Parser p) = Parser p''
    where p'' = memo p'
          p' = concatMap f . p
          f (x, str) = recurParse p [x] str

instance ListLike lst Char => Monad (Parser lbl lst) where
  (Parser p) >>= f = Parser p''
    where p'' = memo p'
          p' = concatMap combine . p
          combine (a, str) = pb str
            where Parser pb = f a

instance ListLike lst Char => MonadPlus (Parser lbl lst)

any :: ListLike lst elt => Parser lbl lst elt
any = Parser p
  where p = uncons [] onCons
        onCons x xs = [(x, xs)]

satisfy :: ListLike lst elt => (elt -> Bool) -> Parser lbl lst elt
satisfy pred = Parser p
  where p = uncons [] onCons
        onCons x xs = case pred x of
                        False -> []
                        True ->  [(x, xs)]

char :: (Eq elt, ListLike lst elt) => elt -> Parser lbl lst elt
char c = satisfy (== c)

oneOf :: (FS.HasFastSet elt, ListLike lst elt) => [elt] -> Parser lbl lst elt
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

sequenceP :: (Traversable t, ListLike lst Char) => t Char -> Parser lbl lst (t Char)
sequenceP seq = (sequenceA . fmap char) seq

parse :: Parser lbl lst a -> lst -> [(a, lst)]
parse (Parser p) = p

simpleParse :: ListLike lst elt => Parser lbl lst a -> lst -> [a]
simpleParse (Parser p) = map fst . filter (null . snd) . p
