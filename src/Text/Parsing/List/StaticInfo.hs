{-# LANGUAGE FlexibleContexts #-}

module Text.Parsing.List.StaticInfo (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                     parse, simpleParse) where

import Prelude hiding (null, any)
import Text.Parsing.StaticInfo
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus, mfilter)
import Data.ListLike
import qualified Data.FastSet as FS
import Data.Char (ord)
import Data.List (foldl')

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

data Parser elt lbl lst a = Parser (lst -> [(a, lst)]) (Maybe (StaticInfo elt))

instance FS.HasFastSet elt => Functor (Parser elt lbl lst) where
  fmap f (Parser p s) = Parser (map (mapFst f) . p) s

instance FS.HasFastFiniteSet elt => Applicative (Parser elt lbl lst) where
  pure x  = Parser p s
   where p str = [(x, str)]
         s = Just (StaticInfo True FS.full)
  (Parser pf sf) <*> ~(Parser pa sa) = Parser p s 
    where p = concatMap combine . pf
          combine (f, str) = map (mapFst f) (pa str)
          s = sequentialCompose sf sa
  (Parser pa sa) <* ~(Parser pb sb) = Parser p s 
    where p = concatMap combine . pa
          combine (a, str) = map (replaceFst a) (pb str)
          s = sequentialCompose sa sb
  (Parser pa sa) *> ~(Parser pb sb) = Parser p s
    where p = concatMap pb . map snd . pa
          s = sequentialCompose sa sb

recurParse :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParse p as str = map (mapFst reverse) (go as str)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

instance (FS.HasFastFiniteSet elt, ListLike lst elt) => Alternative (Parser elt lbl lst) where
  empty = Parser (const []) (Just (StaticInfo False FS.empty))
  (Parser pl sl) <|> (Parser pr sr) = Parser p s
   where p str = pl' str ++ pr' str
         pl' = useStatic sl pl
         pr' = useStatic sr pr
         s = paralellCompose sl sr
  many (Parser p s) = Parser (recurParse p' []) s'
    where p' = useStatic s p
          s' = case s of
                 Nothing -> Nothing
                 Just (StaticInfo _ st) -> Just (StaticInfo True st)
  some (Parser p s) = Parser (concatMap f . p') s
    where f (x, str) = recurParse p' [x] str
          p' = useStatic s p

instance FS.HasFastFiniteSet elt => Monad (Parser elt lbl lst) where
  (Parser pa sa) >>= f = Parser p s
    where p = concatMap combine . pa
          combine (a, str) = pb str
            where Parser pb _ = f a
          s = sequentialCompose sa Nothing

instance (FS.HasFastFiniteSet elt, ListLike lst elt) => MonadPlus (Parser elt lbl lst)

any :: (FS.HasFastFiniteSet elt, ListLike lst elt) => Parser elt lbl lst elt
any = Parser p s
  where p str = uncons [] onCons str
        onCons x xs = [(x, xs)]
        s = Just (StaticInfo False FS.full)

satisfyWithSet :: (FS.HasFastFiniteSet elt, ListLike lst elt) => (elt -> Bool) -> FS.FastSet elt -> Parser elt lbl lst elt
satisfyWithSet pred set = Parser p s
  where p str = uncons [] onCons str
        onCons x xs = if pred x then [(x, xs)] else []
        s = Just (StaticInfo (FS.isEmpty set) set)

satisfy :: (FS.HasFastFiniteSet elt, ListLike lst elt) => (elt -> Bool) -> Parser elt lbl lst elt
satisfy pred = satisfyWithSet pred (FS.filter pred FS.full)

char :: (Eq elt, FS.HasFastFiniteSet elt, ListLike lst elt)  => elt -> Parser elt lbl lst elt
char c = satisfyWithSet (== c) (FS.fromList [c])

oneOf :: (FS.HasFastFiniteSet elt, ListLike lst elt) => [elt] -> Parser elt lbl lst elt
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

sequenceP :: (Eq elt, FS.HasFastFiniteSet elt, ListLike lst elt, Traversable t) => t elt -> Parser elt lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

parse :: Parser elt lbl lst a -> lst -> [(a, lst)]
parse (Parser p _) = p

simpleParse :: ListLike lst elt => Parser elt lbl lst a -> lst -> [a]
simpleParse (Parser p _) = map fst . filter (null . snd) . p
