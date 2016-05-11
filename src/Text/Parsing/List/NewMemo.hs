{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, RecursiveDo #-}

module Text.Parsing.List.NewMemo (Parser, any, satisfy, char, oneOf, decimal, string, label,
                                  parse, simpleParse, parseST, simpleParseST) where 

import Prelude hiding (null, any)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Data.ListLike
import Data.Opaque
import qualified Data.FastSet as FS
import Data.Char (ord)
import Data.List (foldl')
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Control.Monad.Trans.State.Strict (StateT, get, modify, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.MapLike as ML
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Debug.Trace (traceM)


data Parser elt lbl lst a where
  Fail         :: Parser elt lbl lst a
  Empty        :: a -> Parser elt lbl lst a
  Any          :: Parser elt lbl lst elt
  Satisfy      :: (elt -> Bool) -> Parser elt lbl lst elt
  Str          :: lst -> Parser elt lbl lst lst
  Fmap         :: (a -> b) -> Parser elt lbl lst a -> Parser elt lbl lst b
  Ap           :: Parser elt lbl lst (a -> b) -> Parser elt lbl lst a -> Parser elt lbl lst b
  DiscardRight :: Parser elt lbl lst a -> Parser elt lbl lst b -> Parser elt lbl lst a
  DiscardLeft  :: Parser elt lbl lst a -> Parser elt lbl lst b -> Parser elt lbl lst b
  Or           :: Parser elt lbl lst a -> Parser elt lbl lst a -> Parser elt lbl lst a
  Many         :: Parser elt lbl lst a -> Parser elt lbl lst [a]
  Some         :: Parser elt lbl lst a -> Parser elt lbl lst [a]
  Bind         :: Parser elt lbl lst a -> (a -> Parser elt lbl lst b) -> Parser elt lbl lst b
  Label        :: lbl -> Parser elt lbl lst a -> Parser elt lbl lst a

instance Functor (Parser elt lbl lst) where
  fmap = Fmap

instance Applicative (Parser elt lbl lst) where
  pure = Empty
  (<*>) = Ap
  (<*) = DiscardRight
  (*>) = DiscardLeft

instance Alternative (Parser elt lbl lst) where
  empty = Fail
  (<|>) = Or
  many = Many
  some = Some

instance Monad (Parser elt lbl lst) where
  (>>=) = Bind

instance MonadPlus (Parser elt lbl lst)

any :: Parser elt lst lbl elt
any = Any

satisfy :: (elt -> Bool) -> Parser elt lbl lst elt
satisfy = Satisfy

char :: Eq elt => elt -> Parser elt lbl lst elt
char c = satisfy (== c)

oneOf :: FS.HasFastSet elt => [elt] -> Parser elt lbl lst elt
oneOf lst = satisfy pred
  where pred c = c `FS.member` set
        set = FS.fromList lst

digit :: Parser Char lbl lst Int
digit = charToDigit <$> oneOf ['0'..'9']
  where charToDigit c = ord c - ord '0'

decimal :: Parser Char lbl lst Integer
decimal = listToNum <$> some digit
  where listToNum = foldl' nextDigit 0
        nextDigit num digit = 10*num + toInteger digit

string = Str

sequenceP :: (Eq elt, Traversable t) => t elt -> Parser elt lbl lst (t elt)
sequenceP seq = (sequenceA . fmap char) seq

label :: lbl -> Parser elt lbl lst b -> Parser elt lbl lst b
label = Label

mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
replaceFst x (_, y) = (x, y)
replaceSnd y (x, _) = (x, y)

recurParse :: (lst -> [(a, lst)]) -> [a] -> lst -> [([a], lst)]
recurParse p as str = map (mapFst reverse) (go as str)
  where go soFar str = concatMap f (p str) ++ [(soFar, str)]
                         where f (x, str') = go (x:soFar) str'

type TranslatorState lbl = HM.HashMap lbl Opaque
type TranslatorMonadT m lbl = StateT (TranslatorState lbl) m
type TranslatorMonad lbl = TranslatorMonadT Identity lbl

translate :: (Eq lbl, Hashable lbl, ListLike lst elt) => Parser elt lbl lst a -> TranslatorMonad lbl (lst -> TranslatorMonad lbl [(a, lst)])
translate parser = case parser of
                     Fail -> return (const (return []))
                     Empty a -> return p
                                  where p lst = return [(a, lst)]
                     Any -> return p
                              where p = uncons onEmpty onCons
                                    onEmpty = return []
                                    onCons hd tl = return [(hd, tl)]
                     Satisfy f -> return p
                                    where p = uncons onEmpty onCons
                                          onEmpty = return []
                                          onCons hd tl = return $! case f hd of
                                                                     False -> []
                                                                     True  -> [(hd, tl)]
                     Str str -> return p
                                  where p = dropPrefix str onFailure onSuccess
                                        onFailure = return []
                                        onSuccess rest = return [(str, rest)]
                     Fmap f parser' -> do
                                         p <- translate parser'
                                         return (fmap (map (mapFst f)) . p)
                     Ap parserF parserA -> do
                                             pf <- translate parserF
                                             pa <- translate parserA
                                             let combine (f, str) = map (mapFst f) <$> pa str
                                             let p str = pf str >>= concatMapM combine
                                             return p
                     DiscardRight parserA parserB -> do
                                                       pa <- translate parserA
                                                       pb <- translate parserB
                                                       let combine (a, str) = map (replaceFst a) <$> pb str
                                                       let p str = pa str >>= concatMapM combine
                                                       return p
                     DiscardLeft parserA parserB -> do
                                                      pa <- translate parserA
                                                      pb <- translate parserB
                                                      let p str = pa str >>= concatMapM (pb . snd)
                                                      return p
                     Or parserL parserR -> do
                                             pl <- translate parserL
                                             pr <- translate parserR
                                             let p str = (++) <$> pl str <*> pr str
                                             return p
                     Many parser' -> do
                                       p <- translate parser'
                                       return (recurParseM p [])
                     Some parser' -> do
                                       p <- translate parser'
                                       let f (x, str) = recurParseM p [x] str
                                       let p' str = p str >>= concatMapM f
                                       return p'   
                     Bind parserA f -> do
                                         pa <- translate parserA
                                         let combine (a, str) = do
                                                                  pb <- translate (f a)
                                                                  pb str
                                         let p str = pa str >>= concatMapM combine
                                         return p
                     Label label parser' -> do
                                              translationCache <- get
                                              case HM.lookup label translationCache of
                                                Just op -> return (fromOpaque op)
                                                Nothing  -> mdo
                                                              modify (HM.insert label (toOpaque p))
                                                              p <- translate parser'
                                                              return p

parse :: (Eq lbl, Hashable lbl, ListLike lst elt) => Parser elt lbl lst a -> lst -> [(a, lst)]
parse parser lst = runIdentity $ flip evalStateT HM.empty $ do
                                                             p <- translate parser
                                                             p lst

simpleParse :: (Eq lbl, Hashable lbl, ListLike lst elt) => Parser elt lbl lst a -> lst -> [a]
simpleParse p = map fst . filter (null . snd) . parse p

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

recurParseM :: Monad m => (lst -> m [(a, lst)]) -> [a] -> lst -> m [([a], lst)]
recurParseM p as str = map (mapFst reverse) <$> go as str
  where go soFar str = do
                         xs <- p str
                         let f (x, str') = go (x:soFar) str'
                         init <- concatMapM f xs
                         return (init ++ [(soFar, str)])

type TranslatorMonadST s lbl = TranslatorMonadT (ST s) lbl

translateST :: forall s map lbl lst elt a. (ML.MapLike map lst, Eq lbl, Hashable lbl, ListLike lst elt)  => (forall v. map v) -> Parser elt lbl lst a -> TranslatorMonadST s lbl (lst -> TranslatorMonadST s lbl [(a, lst)])
translateST emptyCache parser = go parser
  where go :: forall b. Parser elt lbl lst b -> TranslatorMonadST s lbl (lst -> TranslatorMonadST s lbl [(b, lst)])
        go parser = case parser of
                      Fail -> return (const (return []))
                      Empty a -> return p
                                   where p lst = return [(a, lst)]
                      Any -> return p
                               where p = uncons onEmpty onCons
                                     onEmpty = return []
                                     onCons hd tl = return [(hd, tl)]
                      Satisfy f -> return p
                                     where p = uncons onEmpty onCons
                                           onEmpty = return []
                                           onCons hd tl = return $! case f hd of
                                                                     False -> []
                                                                     True  -> [(hd, tl)]
                      Str str -> return p
                                   where p = dropPrefix str onFailure onSuccess
                                         onFailure = return []
                                         onSuccess rest = return [(str, rest)]
                      Fmap f parser' -> do
                                          p <- go parser'
                                          return (fmap (map (mapFst f)) . p)
                      Ap parserF parserA -> do
                                              pf <- go parserF
                                              pa <- go parserA
                                              let combine (f, str) = map (mapFst f) <$> pa str
                                              let p str = pf str >>= concatMapM combine
                                              memo p
                      DiscardRight parserA parserB -> do
                                                        pa <- go parserA
                                                        pb <- go parserB
                                                        let combine (a, str) = map (replaceFst a) <$> pb str
                                                        let p str = pa str >>= concatMapM combine
                                                        memo p
                      DiscardLeft parserA parserB -> do
                                                       pa <- go parserA
                                                       pb <- go parserB
                                                       let p str = pa str >>= concatMapM (pb . snd)
                                                       memo p
                      Or parserL parserR -> do
                                              pl <- go parserL
                                              pr <- go parserR
                                              let p str = (++) <$> pl str <*> pr str
                                              memo p
                      Many parser' -> do
                                        p <- go parser'
                                        memo (recurParseM p [])
                      Some parser' -> do
                                        p <- go parser'
                                        let f (x, str) = recurParseM p [x] str
                                        let p' str = p str >>= concatMapM f
                                        memo p'   
                      Bind parserA f -> do
                                          pa <- go parserA
                                          let combine (a, str) = do
                                                                   pb <- go (f a)
                                                                   pb str
                                          let p str = pa str >>= concatMapM combine
                                          memo p
                      Label label parser' -> do
                                               translationCache <- get
                                               case HM.lookup label translationCache of
                                                 Just op -> return (fromOpaque op)
                                                 Nothing  -> mdo
                                                               modify (HM.insert label (toOpaque p))
                                                               p <- go parser'
                                                               return p
        memo :: forall b. (lst -> TranslatorMonadST s lbl [(b, lst)]) -> TranslatorMonadST s lbl (lst -> TranslatorMonadST s lbl [(b, lst)])
        memo p = do
                   cacheRef <- lift (newSTRef emptyCache)
                   return $ \lst -> do
                                      c <- lift (readSTRef cacheRef)
                                      case ML.lookup lst c of
                                        Just res -> return res
                                        Nothing -> do
                                                     res <- p lst
                                                     lift (modifySTRef' cacheRef (ML.insert lst res))
                                                     return res                          

parseST :: (ML.MapLike map lst, Eq lbl, Hashable lbl, ListLike lst elt) => (forall v. map v) -> Parser elt lbl lst a -> lst -> [(a, lst)]
parseST emptyCache parser lst = runST $ flip evalStateT HM.empty $ do
                                                                      p <- translateST emptyCache parser
                                                                      p lst

simpleParseST :: (ML.MapLike map lst, Eq lbl, Hashable lbl, Show lbl, ListLike lst elt, Show lst) => (forall v. map v) -> Parser elt lbl lst a -> lst -> [a]
simpleParseST emptyCache parser = map fst . filter (null . snd) . parseST emptyCache parser
