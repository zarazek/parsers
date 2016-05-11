{-# LANGUAGE FlexibleContexts #-}

module Text.Parsing.Generic.StaticInfo (Parser, any, satisfy, char, oneOf, decimal, string, sequenceP,
                                        parse, simpleParse) where

import Prelude hiding (null, any)
import Text.Parsing.StaticInfo
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

data Parser m elt lbl lst a = Parser (lst -> m (a, lst)) (Maybe (StaticInfo elt))

instance (Functor m, FS.HasFastSet elt) => Functor (Parser m elt lbl lst) where
  fmap f (Parser p s) = Parser (fmap (mapFst f) . p) s

instance (Monad m, FS.HasFastFiniteSet elt) => Applicative (Parser m elt lbl lst) where
  pure x  = Parser p s
   where p str = pure (x, str)
         s = Just (StaticInfo True FS.full)
  (Parser pf sf) <*> ~(Parser pa sa) = Parser p s 
    where p = pf >=> combine
          combine (f, str) = fmap (mapFst f) (pa str)
          s = sequentialCompose sf sa
  (Parser pa sa) <* ~(Parser pb sb) = Parser p s 
    where p = pa >=> combine
          combine (a, str) = fmap (replaceFst a) (pb str)
          s = sequentialCompose sa sb
  (Parser pa sa) *> ~(Parser pb sb) = Parser p s
    where p = pa >=> (pb . snd)
          s = sequentialCompose sa sb

recurParse :: (Monad m, Alternative m) => (lst -> m (a, lst)) -> [a] -> lst -> m ([a], lst)
recurParse p as str = mapFst reverse <$> go as str
  where go soFar str = (p str >>= f) <|> (pure (soFar, str))
          where f (x, str') = go (x:soFar) str'

instance (Monad m, Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => Alternative (Parser m elt lbl lst) where
  empty = Parser (const empty) (Just (StaticInfo False FS.empty))
  (Parser pl sl) <|> (Parser pr sr) = Parser p s
   where p str = pl' str <|> pr' str
         pl' = useStatic sl pl
         pr' = useStatic sr pr
         s = paralellCompose sl sr
  many (Parser p s) = Parser manyP manyS
    where manyP = recurParse p' []
          p' = useStatic s p
          manyS = case s of
                    Nothing -> Nothing
                    Just (StaticInfo _ st) -> Just (StaticInfo True st)
  some (Parser p s) = Parser someP s
    where someP = p' >=> f
          f (x, str) = recurParse p' [x] str
          p' = useStatic s p

instance (Monad m, FS.HasFastFiniteSet elt) => Monad (Parser m elt lbl lst) where
  (Parser pa sa) >>= f = Parser p s
    where p = pa >=> combine
          combine (a, str) = pb str
            where Parser pb _ = f a
          s = sequentialCompose sa Nothing

instance (Monad m, Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => MonadPlus (Parser m elt lbl lst)

any :: (Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => Parser m elt lbl lst elt
any = Parser p s
  where p str = uncons empty onCons str
        onCons x xs = pure (x, xs)
        s = Just (StaticInfo False FS.full)

satisfyWithSet :: (Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => (elt -> Bool) -> (FS.FastSet elt) -> Parser m elt lbl lst elt
satisfyWithSet pred set = Parser p s
  where p str = uncons empty onCons str
        onCons x xs = case pred x of
                        True  -> pure (x, xs)
                        False -> empty
        s = Just (StaticInfo (FS.isEmpty set) set)

satisfy :: (Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => (elt -> Bool) -> Parser m elt lbl lst elt
satisfy pred = satisfyWithSet pred (FS.filter pred FS.full)

char :: (Alternative m, Eq elt, FS.HasFastFiniteSet elt, ListLike lst elt)  => elt -> Parser m elt lbl lst elt
char c = satisfyWithSet (== c) (FS.fromList [c])

oneOf :: (Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => [elt] -> Parser m elt lbl lst elt
oneOf lst = satisfyWithSet pred set
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: (Alternative m, ListLike lst Char) => Parser m Char lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: (Monad m, Alternative m, ListLike lst Char) => Parser m Char lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string :: (Alternative m, FS.HasFastFiniteSet elt, ListLike lst elt) => lst -> Parser m elt lbl lst lst
string str = Parser p s
  where p = dropPrefix str empty onSuccess
        onSuccess rest = pure (str, rest)
        s = uncons onEmpty onCons str
        onEmpty = Just (StaticInfo True FS.full)
        onCons c _ = Just (StaticInfo False (FS.fromList [c]))

sequenceP :: (Monad m, Alternative m, Eq elt, FS.HasFastFiniteSet elt, ListLike lst elt, Traversable t) => t elt -> Parser m elt lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

parse :: Parser m elt lbl lst a -> lst -> m (a, lst)
parse (Parser p _) = p

simpleParse :: (MonadPlus m, ListLike lst elt) => Parser m elt lbl lst a -> lst -> m a
simpleParse (Parser p _) = fmap fst . mfilter (null . snd) . p
