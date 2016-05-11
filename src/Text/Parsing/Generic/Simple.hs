{-# LANGUAGE FlexibleContexts #-}

module Text.Parsing.Generic.Simple (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                    parse, simpleParse) where

import Prelude hiding (null, any)
import Control.Applicative (Alternative(..))
import Control.Monad ((>=>), MonadPlus, mfilter)
import Data.ListLike
import qualified Data.FastSet as FS
import Data.Char (ord)
import Data.List (foldl')

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

newtype Parser m lbl lst a = Parser (lst -> m (a, lst))

instance Functor m => Functor (Parser m lbl lst) where
  fmap f (Parser p) = Parser (fmap (mapFst f) . p)

instance Monad m => Applicative (Parser m lbl lst) where
  pure x  = Parser (\str -> pure (x, str))
  (Parser pf) <*> (Parser pa) = Parser (pf >=> combine)
    where combine (f, str) = fmap (mapFst f) (pa str)
  (Parser pa) <* (Parser pb) = Parser (pa >=> combine)
    where combine (a, str) = fmap (replaceFst a) (pb str)
  (Parser pa) *> (Parser pb) = Parser (pa >=> (pb . snd))

recurParse :: (Monad m, Alternative m) => (lst -> m (a, lst)) -> [a] -> lst -> m ([a], lst)
recurParse p as str = mapFst reverse <$> go as str
  where go soFar str = (p str >>= f) <|> (pure (soFar, str))
          where f (x, str') = go (x:soFar) str'

instance (Monad m, Alternative m) => Alternative (Parser m lbl lst) where
  empty = Parser (const empty)
  (Parser pl) <|> (Parser pr) = Parser (\str -> pl str <|> pr str)
  many (Parser p) = Parser (recurParse p [])
  some (Parser p) = Parser (p >=> f)
    where f (x, str) = recurParse p [x] str

instance Monad m => Monad (Parser m lbl lst) where
  (Parser p) >>= f = Parser (p >=> combine)
    where combine (a, str) = pb str
            where Parser pb = f a

instance (Monad m, Alternative m) => MonadPlus (Parser m lbl lst)

any :: (Alternative m, ListLike lst elt) => Parser m lbl lst elt
any = Parser p
  where p str = uncons empty onCons str
        onCons x xs = pure (x, xs)

satisfy :: (Alternative m, ListLike lst elt) => (elt -> Bool) -> Parser m lbl lst elt
satisfy pred = Parser p
  where p str = uncons empty onCons str
        onCons x xs = case pred x of
                        True  -> pure (x, xs)
                        False -> empty

char :: (Eq elt, Alternative m, ListLike lst elt) => elt -> Parser m lbl lst elt
char c = satisfy (== c)

oneOf :: (Alternative m, ListLike lst elt, FS.HasFastSet elt) => [elt] -> Parser m lbl lst elt
oneOf lst = satisfy pred
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: (Alternative m, ListLike lst Char) => Parser m lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: (Alternative m, Monad m, ListLike lst Char) => Parser m lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string :: (Alternative m, ListLike lst elt) => lst -> Parser m lbl lst lst
string str = Parser p 
  where p = dropPrefix str empty onSuccess
        onSuccess rest = pure (str, rest)

sequenceP :: (Alternative m, Monad m, Traversable t, Eq elt, ListLike lst elt) => t elt -> Parser m lbl lst (t elt)
sequenceP str = (sequenceA . fmap char) str

parse :: ListLike lst elt => Parser m lbl lst a -> lst -> m (a, lst)
parse (Parser p) = p

simpleParse :: (MonadPlus m, ListLike lst elt) => Parser m lbl lst a -> lst -> m a
simpleParse (Parser p) = fmap fst . mfilter (null . snd) . p
