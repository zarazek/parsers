{-# LANGUAGE FlexibleContexts #-}

module Text.Parsing.Maybe.Simple (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                  parse, simpleParse) where

import Prelude hiding (null, any)
import Control.Applicative (Alternative(..))
import Control.Monad ((>=>), MonadPlus, mfilter)
import Data.ListLike
import qualified Data.FastSet as FS
import Data.Char (ord)
import qualified Data.List as L (null, foldl')

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

newtype Parser lbl lst a = Parser (lst -> Maybe (a, lst))

instance Functor (Parser lbl lst) where
  fmap f (Parser p) = Parser (fmap (mapFst f) . p)

instance Applicative (Parser lbl lst) where
  pure x  = Parser (\str -> Just (x, str))
  (Parser pf) <*> (Parser pa) = Parser (pf >=> combine)
    where combine (f, str) = mapFst f <$> pa str
  (Parser pa) <* (Parser pb) = Parser (pa >=> combine)
    where combine (a, str) = replaceFst a <$> pb str
  (Parser pa) *> (Parser pb) = Parser (pa >=> (pb . snd))

recurParse p as str = mapFst reverse <$> go as str
  where go soFar str = case (p str) of
                         Nothing        -> Just (soFar, str)
                         Just (x, str') -> go (x:soFar) str'

instance Alternative (Parser lbl lst) where
  empty = Parser (const empty)
  (Parser pl) <|> (Parser pr) = Parser (\str -> pl str <|> pr str)
  many (Parser p) = Parser (recurParse p [])
  some (Parser p) = Parser p' where
    p' str = case p str of
               Nothing        -> Nothing
               Just (x, str') -> recurParse p [x] str'

instance Monad (Parser lbl lst) where
  (Parser pa) >>= f = Parser (pa >=> combine)
    where combine (a, str) = pb str
            where Parser pb = f a

instance MonadPlus (Parser lbl lst)

any :: ListLike lst elt => Parser lbl lst elt
any = Parser p
  where p str = uncons Nothing onCons str
        onCons x xs = Just (x, xs)

satisfy :: ListLike lst elt => (elt -> Bool) -> Parser lbl lst elt
satisfy pred = Parser p
  where p str = uncons empty onCons str
        onCons x xs = case pred x of 
                        True  -> Just (x, xs)
                        False -> Nothing

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
  where listToNum = L.foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string :: ListLike lst elt => lst -> Parser lbl lst lst
string str = Parser p 
  where p = dropPrefix str Nothing onSuccess
        onSuccess rest = Just (str, rest)

sequenceP :: (Traversable t, Eq elt, ListLike lst elt) => t elt -> Parser lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

parse :: ListLike lst elt => Parser lbl lst a -> lst -> Maybe (a, lst)
parse (Parser p) = p

simpleParse :: ListLike lst elt => Parser lbl lst a -> lst -> Maybe a
simpleParse (Parser p) = fmap fst . mfilter (null . snd) . p
